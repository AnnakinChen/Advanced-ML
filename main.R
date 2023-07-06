library(shiny)
library(shinydashboard)
library(fUnitRoots)
library(MASS)  #lda
library(glmnet)  #lasso,ridge
library(readr)
library(survival) #survival analysis
library(quantreg)
library(pROC)
library(ggplot2)
library(e1071)    #SVM
library(tidyr)
library(DT)
library(mlr3verse)
library(rpart)   #决策树
library(rpart.plot)
library(paradox)
library(dplyr)
library(precrec)
library(quadprog)
library(ranger)  #随机森林
library(rugarch)
library(tseries)
library(mixAR)
library(gbutils)
library(mnormt)
library(uGMAR)
library(adabag)  #adaboost
library(xgboost)
library(lightgbm)


source('ui.R')
source('server.R')
source('diy.R')
source('dda.R')

shinyApp(ui,server)



