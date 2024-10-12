```{r}


logistic_model <- NULL
decision_model <- NULL
# Define UI for application that draws different plot
ui <- navbarPage(
  titlePanel("Visualisation of death in country/region during 1990 - 2019"),
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
```