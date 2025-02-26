# CITS4009 Project — Exploratory Data Analytics and Predictive Modelling

### Result: 75/100

## Outline of the project
The project focuses on performing exploratory data analysis (EDA) and predictive modeling on a specific dataset. For this project, the given dataset is the Countries and Death causes dataset. The analysis aims to uncover patterns in the data and develop predictive models for classification and clustering tasks.

## Submission files included:
- Knitted R markdown (HTML).
- R scripted for Shiny app.

## Exploratory Data Analysis and Modelling implementation
- Data Preparation:
  + Cleaned and transformed the data, including data types conversions, removing duplicated values, handling missing values, labelling data, and merging datasets where necessary.
  + Developed interactive visualisation using Shiny App for the data for better data understanding and exploration. 
- Buid models, including classification and clustering.
  + Classification: Implemented Decision Tree and Logistic Regression models. With each model type, there are two variations, one with full features and one model with selected features. Evaluated model performance using ROC plots and results of precision, recall, f1, accuracy.
  + Clustering: Built a clustering model and visualised the results using a dendrogram. Applied Principle components - dimension reduction to visualise the clusters on 2D graph. Selected the optimal number of clusters (k) by using the Within Sum of Square (WSS) plot. 
