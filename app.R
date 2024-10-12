# Install all the required packages for data processing, data transformation and data visualization
library(ggplot2)
library(shiny)
library(gridExtra)
library(reshape2)
library(readxl)
library(dplyr)

#insert the data
path<-url("https://lms.uwa.edu.au/bbcswebdav/pid-3998436-dt-content-rid-47695412_1/courses/CITS4009_SEM-2_2024/Countries%20and%20death%20causes.csv")
data$Entity<-as.factor(data$Entity)
data$Code<-as.factor(data$Code)
data$Year<-as.factor(data$Year)

region_mask <- grepl("(WB)|(WHO)|^World|^G20|^OECD", data$Entity)
regions<-data[region_mask,]
countries<-data[!region_mask,]

#here is my file path, please change to your filepath to where you downloaded the file to run the code below
path<-"/Users/henrytran/Documents/UWA_Master-Data-Science/CITS4009/Project/"

#create a path that can access and extractn the data
country_groups_file<- paste0(path, "World Bank Country Groups.xlsx")
country_groups_dataset<-read_excel(country_groups_file, sheet = "List of economies")
country_groups_dataset<-country_groups_dataset[,c(1,2,4)]

countries_merged<-merge(countries, country_groups_dataset, by.x="Code", by.y="Code", all.x=TRUE)
#we keep the remaining countries
countries_merged<-merge(countries, country_groups_dataset, by.x="Code", by.y="Code")

#for the data, group the data into entity and income groups, then calculate the total death of each death causes of each country
names(countries_merged)[names(countries_merged) == "Income group"] <- "Income.group"

total_countries_death <- countries_merged %>% group_by(Entity, Income.group) %>% summarise(across(c("Outdoor.air.pollution": "Iron.deficiency"), sum))

label_total_countries_death<-total_countries_death
label_total_countries_death$Label<-ifelse(total_countries_death$Income.group == "High income", 1, 0)
label_total_countries_death$Label<-as.factor(label_total_countries_death$Label)

set.seed(12345)
random_mask<-runif(nrow(label_total_countries_death))<0.8 #80% of data will be used for training, 20% of data will be used for testing
train_data<-label_total_countries_death[random_mask,]
test_data<-label_total_countries_death[!random_mask,]

train_data<-train_data[,3:ncol(train_data)]
test_data<-test_data[,3:ncol(test_data)]

responses<- colnames(label_total_countries_death) %in% c("Entity", "Income.group", "Label") # create a mask to filter out the responses and the features 
all_features<-colnames(label_total_countries_death)[!responses]

mkPredC<-function(outCol, varCol, appCol){ #training outcomes, training variable and prediction variable
  #we convert all the data to vector for easier calculation
  outCol <- as.vector(outCol) 
  varCol <- as.vector(varCol)
  appCol <- as.vector(appCol)
  pOne<-sum(outCol==1)/length(outCol) #the probability of the outcome is 1 during training
  vTab<-table(outCol, varCol) #the table of training outcomes and training variable
  pOneWv<-(vTab[1,]+ 1.0e-3 *pOne)/(colSums(vTab)+ 1.0e-3) #how often the outcome is 1 with conditioned on levels of training variable
  pred<-pOneWv[appCol] #make prediction by looking up levels of appCol
  pred[is.na(pred)]<-pOne #add prediction for appCol that were unknown when training
  pred
}

mkPredN<- function(outCol, varCol, appCol){
  varCol <- as.numeric(varCol)  #we convert all the data to numerical to use for quantile function
  appCol <- as.numeric(appCol)  #we convert all the data to numerical to use for quantile function
  cuts<-unique(quantile(varCol, probs = seq(0,1,0.1), na.rm=T)) #we convert the numerical data to groups of categorical data
  varC<-cut(varCol, cuts) #we applied the cuts to the training variables
  appC<-cut(appCol, cuts) #we applied the cuts to the testing variable
  mkPredC(outCol, varC, appC)
}

#this is the code followed from the lecture, with some modification
#import the ROCR library to create calcAUC function used for calculating single-variate model's AUCs 
library('ROCR')
calcAUC <- function(predcol,outcol) {
  perf <- performance(prediction(predcol,outcol==1),'auc') #we are checking the performance of model predicting the value 1
  as.numeric(perf@y.values)
}

#this is the code followed from the lecture, with some modification
performanceMeasures <- function(pred, truth, name = "model") {
  # Construct confusion matrix ensuring all levels (0 and 1) are present
  ctable <- table(factor(truth, levels = c(0, 1)), factor(pred > 0.5, levels = c(FALSE, TRUE)))
  accuracy <- sum(diag(ctable)) / sum(ctable) #(TP+TN)/(TP+FP+TN+FN)
  precision <- ifelse(sum(ctable[, 2]) == 0, 0, ctable[2, 2] / sum(ctable[, 2])) #we prevent the division with 0, (TP)/(TP+FP)
  recall <- ifelse(sum(ctable[2, ]) == 0, 0, ctable[2, 2] / sum(ctable[2, ])) #we prevent the division with 0, (TP)/(TP+FN)
  f1 <- ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall)) #we prevent the division with 0, (2*precision*recall)/(precision+recall)
  data.frame(model = name, precision = precision, recall = recall, f1 = f1, accuracy = accuracy)
}

pretty_perf_table <- function(model,training,test, feature, model_type) {
  library(pander)
  # setting up Pander Options
  panderOptions("plain.ascii", TRUE)
  panderOptions("keep.trailing.zeros", TRUE)
  panderOptions("table.style", "simple")
  perf_justify <- "lrrrr"
  # comparing performance on training vs. test
  if (model_type == "logistic") {
    pred_train <- predict(model, newdata=training[, feature], type="response") #for logistic regression, we need to set predict type to "response"
    pred_test <- predict(model, newdata=test[, feature], type="response")
  } else if (model_type == "decision_tree") {
    pred_train <- predict(model, newdata=training[, feature], type="prob")[, 2]  # for logistic regression, we need to set predict type to "prob", and take the second column, as the result
    pred_test <- predict(model, newdata=test[, feature], type="prob")[, 2]
  }
  truth_train <- training[["Label"]]
  truth_test <- test[["Label"]]
  
  trainperf_tree <- performanceMeasures(pred_train, truth_train, "logistic, training")
  testperf_tree <- performanceMeasures(pred_test, truth_test, "logistic, test")
  
  perftable <- rbind(trainperf_tree, testperf_tree)
  pandoc.table(perftable, justify = perf_justify)
}

library(rpart)
library(rpart.plot)

#this is the code followed from the lecture, with some modification
library(ROCit)
# Function to plot ROC curves for both logistic regression and decision tree models
plot_roc <- function(predcol1_test, outcol1_test, predcol1_train, outcol1_train, 
                     predcol2_test, outcol2_test, predcol2_train, outcol2_train) {
  #we calcualte the ROC for the first model, which is the logistic regression model
  roc_1_test <- rocit(score = predcol1_test, class = outcol1_test == 1)
  roc_1_train <- rocit(score = predcol1_train, class = outcol1_train == 1)
  #we calcualte the ROC for the first model, which is the decision tree model
  roc_2_test <- rocit(score = predcol2_test, class = outcol2_test == 1)
  roc_2_train <- rocit(score = predcol2_train, class = outcol2_train == 1)
  # Plot test data for both models
  plot(roc_1_test, col = c("blue", "green"), lwd = 2, legend = FALSE, YIndex = FALSE, values = TRUE, asp = 1)
  lines(roc_2_test$TPR ~ roc_2_test$FPR, col = c("red", "green"), lwd = 2)
  lines(roc_1_train$TPR ~ roc_1_train$FPR, col = c("blue", "green"), lwd = 2, lty = 2)
  lines(roc_2_train$TPR ~ roc_2_train$FPR, col = c("red","green"), lwd = 2, lty = 2)
  #we add the legend to classify the training and testing lines for each model
  legend("bottomright", col = c("blue", "red","green"), 
         legend = c("Logistic Test", "Decision Tree Test", "Null Model", "Logistic Train", "Decision Tree Train"),
         lwd = 2,lty = c(1, 1, 2, 2, 2))
}

total_regions_death <- regions %>% group_by(Entity) %>% summarise(across(c("Outdoor.air.pollution": "Iron.deficiency"), sum))
total_regions_death
cluster_data<-total_regions_death

scale_data<-scale(cluster_data[, 2:ncol(cluster_data)]) # this will be the data where each column has 0 mean and unit standard deviation

library('grDevices')
#this function will return the vector of points in each convex hull
find_convex_hull <- function(proj2Ddf, groups) {
  do.call(rbind,lapply(unique(groups), FUN = function(c) {
    f <- subset(proj2Ddf, cluster==c);
    f[chull(f),] # this compute the convex hull of a set of points
  }))
}

#these are the code followed from the lecture, with some modification.
#this function will return the squared Euclidean distance of two given points x and y from the cluster centroid
sqr_euDist <- function(x, y) {
  sum((x - y)^2)
}
#this function calculates WSS of a cluster, represented as a n-by-d matrix (n is number of rows and d is the number of column) which contains only points of the cluster.
wss <- function(clustermat) {
  c0 <- colMeans(clustermat)
  sum(apply( clustermat, 1, FUN=function(row) {sqr_euDist(row, c0)} ))
}
#this function will calculate the total WSS.
wss_total <- function(scaled_df, labels) {
  wss.sum <- 0
  k <- length(unique(labels))
  for (i in 1:k)
    wss.sum <- wss.sum + wss(subset(scaled_df, labels == i))
  wss.sum
}
#this function will calculate the total sum of squared (TSS) distance of data points about the mean. This is the same as WSS when the k equals to 1.
tss <- function(scaled_df) {
  wss(scaled_df)
}

calculate_indices <- function(scaled_df, kmax) {
  npts <- nrow(scaled_df)
  wss.value <- numeric(kmax) # create a vector of numeric type
  # wss.value[1] stores the WSS value for k=1 (when all the
  # data points form 1 large cluster).
  wss.value[1] <- wss(scaled_df)
  d <- dist(scaled_df, method="euclidean")
  pfit <- hclust(d, method="ward.D2")
  for (k in 2:kmax) {
    labels <- cutree(pfit, k=k)
    wss.value[k] <- wss_total(scaled_df, labels)
  }
  bss.value <- tss(scaled_df) - wss.value # this is a vector
  B <- bss.value / (0:(kmax-1)) # also a vector
  W <- wss.value / (npts - 1:kmax) # also a vector
  data.frame(k = 1:kmax, CH_index = B/W, WSS = wss.value)
}
library(gridExtra)

logistic_model <- NULL
decision_model <- NULL
# Define UI for application that draws different plot
ui <- navbarPage(
  "Analysis of number of recorded deaths and causes in countries/regions during 1990 - 2019",
  tabPanel("EDA",
           sidebarLayout(
             sidebarPanel(
               selectInput("plot_type", "Select plot type:", choices = c("Line plot", "Boxplot", "Heat map", "Correlation plot")),
               conditionalPanel(
                 condition = "input.plot_type == 'Line plot'",
                 checkboxInput("compare", "Compare between 2 countries/regions", FALSE),
               ),
               selectInput("region1", "Choose the first country/region:", choices = unique(data$Entity)),
               conditionalPanel(
                 condition = "input.compare == true",
                 selectInput("region2", "Choose the second country/region:", choices = unique(data$Entity))
               ),
               conditionalPanel(
                 condition = "input.plot_type == 'Line plot'",
                 selectInput("reason", "Choose death reason:", choices = names(data)[4:31])
               ),
             ),
             mainPanel(            
               plotOutput("plot1", width = "100%", height = "600px"),
               conditionalPanel(
                 condition = "input.compare == true && input.plot_type == 'Line plot'",  # Display plot2 only if compare is checked
                 plotOutput("plot2", width = "100%", height = "600px"))
             )
           )
  ),
  # Tab 2: Decision Tree Classification Model
  tabPanel("Decision Tree Classification Model",
           sidebarLayout(
             sidebarPanel(
               radioButtons("tree_variable_set", "Select variable set:", choices = c("all_features", "selected_features"))
             ),
             mainPanel(
               plotOutput("decision_tree", width = "100%", height = "400px"),
               tableOutput("decision_tree_perf_table")
               
             )
           )
  ),
  # Tab 3: Logistic Regression Model
  tabPanel("Logistic Regression Classification Model",
           sidebarLayout(
             sidebarPanel(
               radioButtons("logistic_variable_set", "Select variable set:", choices = c("all_features", "selected_features"))
             ),
             mainPanel(
               tableOutput("logistic_perf_table")
               
             )
           )
  ),
  # Tab 4: Plot ROC
  tabPanel("Plots to compare AUC",
           sidebarLayout(
             sidebarPanel(
               radioButtons("variable_set", "Select variable set:", choices = c("all_features", "selected_features"))
             ),
             mainPanel(
               plotOutput("ROC_plot", width = "100%", height = "400px"),
             )
           )
  ),
  # Tab 5: Clustering
  tabPanel("Clustering",
           sidebarLayout(
             sidebarPanel(
               selectInput("clustering_plot_type", "Select plot type:", choices = c("Dendogram", "Cluster Visualisation","WSS plot"))
             ),
             mainPanel(
               plotOutput("clustering_plot", width = "100%", height = "400px"),
             )
           ),
  ),
)


# Define the server
server <- function(input, output){
  # Render the histograms
  output$plot1 <- renderPlot({
    region_data<-data[data$Entity==input$region1,]
    # Plot selected death reason
    if (input$plot_type == 'Line plot'){
      ggplot(region_data, aes_string(x="Year", y=input$reason, group = "Entity")) +
        geom_line(color="darkorange") + 
        labs(x="Year", y = input$reason) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))}
    # Plot the boxplot
    else if (input$plot_type == 'Boxplot'){
      data_melt <- melt(region_data, id.vars = "Year", measure.vars = names(data)[4:31], 
                        variable.name = "death_reason", value.name = "value")
      ggplot(data_melt, aes(x = death_reason, y = value, fill = death_reason)) +
        geom_boxplot() +
        labs(x = "Death Reason", y = "Number of Deaths", title = paste("Boxplot of Death Reasons in", input$region1)) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top")+
        guides(fill = guide_legend(ncol = 3))
    }
    
    # Plot the Heat Map for selected country
    else if (input$plot_type == 'Heat map'){
      data_melt <- melt(region_data, id.vars = "Year", measure.vars = names(data)[4:31], variable.name = "death_reason", value.name = "value")
      # Create heat map
      ggplot(data_melt, aes(x = Year, y = death_reason, fill = value)) + 
        geom_tile() + 
        scale_fill_gradient(low = "aliceblue", high = "blue") +
        scale_x_discrete(breaks = levels(region_data$Year)) +
        labs(x = "Year", y = "Death Reason", title = paste("Heat Map of Death Reasons in", input$region1)) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    else if (input$plot_type == "Correlation plot"){
      # Extract the number of death for each death reasons
      numeric_data <- region_data[,4:31]
      
      # Calculate the correlation for all variables
      correlation <- cor(numeric_data)
      
      # Melt the correlation for visualization
      correlation_melted <- melt(correlation)
      
      # Create the correlation heatmap
      ggplot(correlation_melted, aes(x = Var1, y = Var2, fill = value)) +
        geom_tile() +
        scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limit = c(-1, 1), name = "Correlation") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = "Correlation Plot of Death Reasons", x = "Variables", y = "Variables")
    }
  })
  output$plot2 <- renderPlot({ 
    if (input$compare == TRUE && input$plot_type == 'Line plot'){
      region_data<-data[data$Entity==input$region2,]
      # Plot all death reasons
      ggplot(region_data, aes_string(x="Year", y=input$reason, group ="Entity")) + geom_line(color="darkolivegreen") + labs(x="Year", y = input$reason)
    }
  })
  # Decision Tree classification model
  output$decision_tree <- renderPlot({
    variable_set <- input$tree_variable_set
    if (variable_set == "all_features") {
      decision_model <- rpart(Label ~ ., data = train_data[, c(all_features, "Label")], method = "class",control = rpart.control(cp = 0.01))
    } else {
      decision_model <- rpart(Label ~ ., data = train_data[, c(selected_features, "Label")], method = "class")
    }
    rpart.plot(decision_model)
  })
  output$decision_tree_perf_table <- renderTable({
    # Get predictions from the decision tree model
    variable_set <- input$tree_variable_set
    if (variable_set == "all_features") {
      decision_model <- rpart(Label ~ ., data = train_data[, c(all_features, "Label")], method = "class",control = rpart.control(cp = 0.01))
    } else {
      decision_model <- rpart(Label ~ ., data = train_data[, c(selected_features, "Label")], method = "class")
    }
    table<-pretty_perf_table(decision_model, train_data, test_data, all_features, "decision_tree")
    table
  })
  # 
  output$logistic_perf_table <- renderTable({
    # Get predictions from the logistic regression model
    variable_set <- input$logistic_variable_set
    if (variable_set == "all_features") {
      formula<-paste("Label", paste(all_features, collapse=" + "), sep=" ~ ")
      logistic_model<-glm(formula=formula, data=train_data, family=binomial(link="logit"))
    } else {
      formula<-paste("Label", paste(selected_features, collapse=" + "), sep=" ~ ")
      logistic_model <- glm(formula=formula, data=train_data, family=binomial(link="logit"))
    }
    table<-pretty_perf_table(logistic_model, train_data, test_data, all_features, "logistic")
    table
  })
  #ROC plot
  output$ROC_plot <- renderPlot({
    # Get predictions from the logistic regression model
    variable_set <- input$variable_set
    if (variable_set == "all_features") {
      formula<-paste("Label", paste(all_features, collapse=" + "), sep=" ~ ")
      logistic_model<-glm(formula=formula, data=train_data, family=binomial(link="logit"))
      pred_log_test <- predict(logistic_model, newdata = test_data, type = "response")
      pred_log_train <- predict(logistic_model, newdata = train_data, type = "response")
      #decision tree model (using probability)
      decision_model <- rpart(Label ~ ., data = train_data[, c(all_features, "Label")], method = "class",control = rpart.control(cp = 0.01))
      pred_tree_test <- predict(decision_model, newdata = test_data, type = "prob")[, 2]
      pred_tree_train <- predict(decision_model, newdata = train_data, type = "prob")[, 2]
      
      plot_roc(pred_log_test, test_data$Label, pred_log_train, train_data$Label, pred_tree_test, test_data$Label, pred_tree_train, train_data$Label)
    } else {
      formula<-paste("Label", paste(selected_features, collapse=" + "), sep=" ~ ")
      logistic_model<-glm(formula=formula, data=train_data, family=binomial(link="logit"))
      pred_log_test <- predict(logistic_model, newdata = test_data, type = "response")
      pred_log_train <- predict(logistic_model, newdata = train_data, type = "response")
      #decision tree model (using probability)
      decision_model <- rpart(Label ~ ., data = train_data[, c(selected_features, "Label")], method = "class",control = rpart.control(cp = 0.01))
      pred_tree_test <- predict(decision_model, newdata = test_data, type = "prob")[, 2]
      pred_tree_train <- predict(decision_model, newdata = train_data, type = "prob")[, 2]
      
      plot_roc(pred_log_test, test_data$Label, pred_log_train, train_data$Label, pred_tree_test, test_data$Label, pred_tree_train, train_data$Label)
    }
  })
  #clustering
  output$clustering_plot <- renderPlot({
    clustering_plot_type <- input$clustering_plot_type
    d<-dist(scale_data, method="euclidean")
    scale_data<-scale(cluster_data[, 2:ncol(cluster_data)])
    pfit<-hclust(d,method="ward.D2")
    if (clustering_plot_type == "Dendogram") {
      par(cex=0.5, mar=c(5, 5, 5, 5)) #we adjust the size of dendogram to be bigger and the labels be smaller
      plot(pfit, labels = cluster_data$Entity, main="Cluster Dendogram for Death Causes") #we perform a hierarchial clustering
      rect.hclust(pfit, k=3) #we will divide into 3 groups
    } else if (clustering_plot_type == "Cluster Visualisation") {
      groups <- cutree(pfit, k=3)
      
      princ <- prcomp(scale_data) #we calculate the principal components of scaled_df
      nComp <- 2
      project2D <- as.data.frame(predict(princ, newdata=scale_data)[,1:nComp]) #we put the scale data on the principal components dataframe
      hclust.project2D <- cbind(project2D, cluster=as.factor(groups), country=cluster_data$Entity)
      
      hclust.hull <- find_convex_hull(hclust.project2D, groups)
      
      ggplot(hclust.project2D, aes(x=PC1, y=PC2)) + geom_point(aes(shape=cluster, color=cluster)) + geom_text(aes(label=country, color=cluster), hjust=0, vjust=1, size=3) + geom_polygon(data=hclust.hull, aes(group=cluster, fill=as.factor(cluster)),alpha=0.4, linetype=0) + theme(text=element_text(size=20))
    } else{ #WSS plot
      crit.df <- calculate_indices(scale_data, 10)
      ggplot(crit.df, aes(x=k, y=WSS), color="blue") + geom_point() + geom_line(colour="blue") + scale_x_continuous(breaks=1:10, labels=1:10) +     theme(text=element_text(size=20))
    }
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)