# Advanced-ML with Rshiny
The url of online advanced machine learning platform is listed below:  
(url: https://skywalkerchen.shinyapps.io/AdvancedML/)  

In this shiny dashboard, I implement some advanced machine learning model.
The word 'advanced' is true when compared with Basic ML.  
(url: https://anakinchen.shinyapps.io/BasicML)  

These models include linear models, SVM, Naive Bayes Classfier, 
Decision Tree, Ensemble Learning models as well as some financial models.
A content of ML models which is supported
is listed on the left side bar of dashboard. For convenience, 
I also list them below:
1. Linear models
- Elastic Net Regression
- Quantile Regression
- Principal Component Regression
- Linear Discriminant Analysis
- Nonparametric Regression
- Survival Analysis
2. Support Vector Machine
3. Naive Bayes Classfier
4. Desicion Tree
5. Ensemble Learning
- Random Forest
- Adaboost
- Xgboost
- LightGBM
6. Financial Statistics Models
- VaR and ES
- Modern Portfolio Theory
- CAPM
- Time Series (ARIMA, GARCH, MAR and GSMAR)  

Please bear in mind that some models may not belong to linear models. 
However, for convenience, I still put them together.
Basically, this dashboard can achieve classification task and regression task
as well as some financial modelling tasks.  

I give some data for testing this dashboard.  

The 'pimatr(e).csv' can be used for classification.  
The 'Bostontr(e).csv' is for regression task.  
The 'capm.csv', 'Apple.csv', 'IBM.csv' can be used for Financial Modelling.  

Please put all files in same path and run 'main.R'.  
The 'ui.R' is user interface, 'server.R' defines server logic 
while 'diy.R' and 'dda.R' give some functions that cannot be found
in R and R packages (from my view this is true at least).  

I would appreciate a lot if you find any mistake and comment it or email it to me.




 
