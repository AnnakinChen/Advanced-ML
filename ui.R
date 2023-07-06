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

ui<-dashboardPage(
  title = 'Advanced ML Platform',
  dashboardHeader(title = 'AdvancedML'),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        tags$head(
          tags$style(
            HTML(
              "#tabl_f1_progress{margin-bottom:-25px;}"
            )
          )
        ),
        text = 'Advanced Linear Model',
        fileInput('tabl_f1','Regression data'),
        menuSubItem(text = 'Elastic Net Regression',tabName = 'tabl1'),
        menuSubItem(text = 'Quantile Regression',tabName = 'tabl3'),
        menuSubItem(text = 'PCR',tabName = 'tabl4'),
        menuSubItem(text = 'LDA',tabName = 'tabl5'),
        menuSubItem(text = 'Nonparametric',tabName = 'tabl6'),
        menuSubItem(text = 'Survival Analysis',tabName = 'tabl7')
      ),
      menuItem(
        text = 'Support Vector Machine',tabName = 'tabs1'
      ),
      menuItem(
        text = 'Naive Bayes Classfier',tabName = 'tabn1'
      ),
      menuItem(
        text = 'Decision Tree',tabName = 'tabd1'
      ),
      menuItem(
        text = 'Ensemble learning',
        menuSubItem('Random Forest',tabName = 'tabe1'),
        menuSubItem('Adaboost',tabName = 'tabe2'),
        menuSubItem('XGBoost',tabName = 'tabe3'),
        menuSubItem('LightGBM',tabName = 'tabe4')
      ),
      menuItem(
        text = 'Financial Statistics Modelling',
        menuSubItem('VaR & ES',tabName = 'tabf4'),
        menuSubItem('Modern Portfolio Theory',tabName = 'tabf3'),
        menuSubItem('CAPM',tabName = 'tabf1'),
        menuSubItem('Time Series',tabName = 'tabf2')
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = 'tabl1',
        fluidRow(
          box(
            width = 12,
            column(
              width=2,
              selectInput('tabl1_c1','Choose x variables',choices = NULL,multiple = T)
            ),
            column(
              width=2,
              selectInput('tabl1_c2','Choose y variable',choices = NULL)
            ),
            column(
              width=1,
              textInput('tabl1_t1','alpha',value = 1)
            ),
            column(
              width=2,
              textInput('tabl1_t2','lambda begin',value = 0.1)
            ),
            column(
              width=2,
              textInput('tabl1_t3','lambda end',value = 2)
            ),
            column(
              width=1,
              textInput('tabl1_t4','num',value = 500)
            ),
            column(
              width=2,
              selectInput('tabl1_c4','s',choices = c('lambda.min','lambda.1se'))
            ),
            fluidRow(
              column(
                width = 12,
                align="center",
                style='margin-top:-5px',
                actionButton('tabl1_a1',tags$div(strong('Submit')))
              )
            )
          )
        ),
        fluidRow(
          box(
            width=2,
            textInput('tabl1_t5','set.seed'),
            selectInput('tabl1_c3','Choose object',choices = c("lambda","cvm","cvsd","cvup","cvlo","nzero",
                                                               "call","name","glmnet.fit","lambda.min","lambda.1se","index"),
                        selected = "lambda.min"),
            fileInput('tabl1_f1','Upload prediction file'),
            textInput('tabl1_t7','Type lambda'),
            textInput('tabl1_t6','Type real y'),
            actionButton('tabl1_a2','Submit',class='btn',style='margin-bottom:10px;'),
            verbatimTextOutput('tabl1_print4'),
            downloadButton('tabl1_d1')
          ),
          column(
            width = 3,
            verbatimTextOutput('tabl1_print1')
          ),
          column(
            width = 5,
            verbatimTextOutput('tabl1_print2')
          ),
          column(
            width = 2,
            verbatimTextOutput('tabl1_print3')
          )
        )
      ),
      tabItem(
        tabName = 'tabl3',
        fluidRow(
          box(
            width=2,
            selectInput('tabl3_c1','Choose x variables',choices = NULL,multiple = T),
            selectInput('tabl3_c2','Choose y variable',choices = NULL),
            textInput('tabl3_t1','Type tau'),
            actionButton('tabl3_a1','Train'),
            fileInput('tabl3_f1','Upload prediction data'),
            actionButton('tabl3_a2','Predict',class='btn',style='margin-bottom:10px;'),
            verbatimTextOutput('tabl3_p3'),
            downloadButton('tabl3_d1')
          ),
          column(
            width=6,
            verbatimTextOutput('tabl3_p1')
          ),
          column(
            width=4,
            verbatimTextOutput('tabl3_p2')
          )
        )
      ),
      tabItem(
        tabName = 'tabl4',
        fluidRow(
          box(
            title = 'PCA',
            width=3,
            fileInput('tabl4_f1',label = 'Upload a file'),
            selectInput('tabl4_c1',label = 'Choose x variable(s)',choices = NULL,multiple = T),
            actionButton('tabl4_a1',label = 'Train')
          ),
          box(
            title = 'Fitted Model',
            width = 9,
            collapsible = T,
            verbatimTextOutput('tabl4_p1')
          )
        ),
        fluidRow(
          box(
            title = 'PCR',
            width = 3,
            selectInput('tabl4_c2',label = 'Choose y variable',choices = NULL),
            textInput('tabl4_t1','Number of components'),
            actionButton('tabl4_a2',label = 'Train')
          ),
          box(
            title = 'Fitted Model',
            width = 9,
            collapsible = T,
            verbatimTextOutput('tabl4_p2')
          )
        ),
        fluidRow(
          box(
            width = 3,
            title = 'Predict',
            fileInput('tabl4_f2','Upload a file'),
            actionButton('tabl4_a3',label = 'Predict',class='btn',style="margin-bottom:10px"),
            verbatimTextOutput('tabl4_p4'),
            downloadButton('tabl4_d1')
          ),
          box(
            width = 9,
            verbatimTextOutput('tabl4_p3')
          )
        )
      ),
      tabItem(
        tabName = 'tabl5',
        fluidRow(
          box(
            title = 'Training',
            width=3,
            fileInput('tabl5_f1',label = 'Upload a file'),
            selectInput('tabl5_c1',label = 'Choose x variable(s)',choices = NULL,multiple = T),
            selectInput('tabl5_c2',label = 'Choose y variable',choices = NULL),
            actionButton('tabl5_a1',label = 'Train')
          ),
          box(
            title = 'Fitted Model',
            width = 9,
            collapsible = T,
            verbatimTextOutput('tabl5_p1')
          )
        ),
        fluidRow(
          box(
            title = 'Prediction',
            width = 3,
            fileInput('tabl5_f2','Upload a file'),
            actionButton('tabl5_a2',label = 'Predict'),
            downloadButton('tabl5_d1','Download Result')
          ),
          box(
            width = 9,
            collapsible = T,
            verbatimTextOutput('tabl5_p2')
          )
        ),
        fluidRow(
          box(
            width=3,
            selectInput('tabl5_c3',label = 'Choose real y',choices = NULL),
            actionButton('tabl5_a3','Submit',class='btn',style='margin-bottom:10px'),
            br(),
            verbatimTextOutput('tabl5_p3')
          ),
          box(
            width=9,
            column(
              width=6,
              plotOutput('tabl5_plot1')
            ),
            column(
              width=6,
              plotOutput('tabl5_plot2')
            )
          )
        ),
        p('Alternatively, a discriminant analysis by Mahalanobis distance can be considered. 
          You do not need to reupload data, just click the button below and previous data will be used again.',style='font-size:19px;'),
        fluidRow(
          column(
            width=3,
            textInput('tabl5_text1','Type real y'),
            actionButton('tabl5_a4','Proceed',class='btn',style='margin-bottom:10px'),
            br(),
            verbatimTextOutput('tabl5_p4')
          ),
          box(
            width=9,
            verbatimTextOutput('tabl5_p5')
          )
        )
      ),
      tabItem(
        tabName = 'tabl6',
        fluidRow(
          box(
            width = 3,
            fileInput('tabl6_f1',label = 'Upload Training set'),
            fileInput('tabl6_f2',label = 'Upload testing set'),
            selectInput('tabl6_c1',label = 'Choose x variable',choices = NULL),
            selectInput('tabl6_c2',label = 'Choose y variable',choices = NULL),
            selectInput('tabl6_c3',label = 'Choose estimator',choices = c('local average','Nadaraya-Watson','local polynomial')),
            textInput('tabl6_t1',label = 'Type bandwidth',value = 1),
            textInput('tabl6_t2',label = 'Type degree p',value = 3,placeholder = 'local polynomial use only'),
            actionButton('tabl6_a1','Submit',class = 'btn',style = 'margin-bottom:12px;'),
            verbatimTextOutput('tabl6_p1')
          ),
          box(
            width = 9,
            verbatimTextOutput('tabl6_p2')
          )
        )
      ),
      tabItem(
        tabName = 'tabl7',
        navbarPage(
          title = 'Step',
          tabPanel(
            title = 'Fit lifetime distribution',
            fluidRow(
              box(
                title = 'Weibull Distribution',
                width = 3,
                fileInput('tabl7_f1',label = 'Upload data'),
                selectInput('tabl7_c1','Choose t',choices = NULL),
                actionButton('tabl7_a1',label = 'Submit')
              ),
              box(
                width = 9,
                verbatimTextOutput('tabl7_p1')
              )
            ),
            fluidRow(
              box(
                title = 'Kaplan-Meier estimate',
                width = 2,
                selectInput('tabl7_c2','Choose t',choices = NULL),
                selectInput('tabl7_c3','Choose status',choices = NULL),
                selectInput('tabl7_c4','Choose result',choices = c('surv','n.risk','n.event','n.censor')),
                textInput('tabl7_t1','Time',value = '2'),
                actionButton('tabl7_a2',label = 'Submit',class = 'btn',style = 'margin-bottom:13px;'),
                verbatimTextOutput('tabl7_p3')
              ),
              box(
                width = 10,
                fluidRow(
                  column(
                    width = 6,
                    verbatimTextOutput('tabl7_p2')
                  ),
                  column(
                    width = 6,
                    plotOutput('tabl7_plot1')
                  )
                )
              )
            )
          ),
          tabPanel(
            title = 'Weibull PH model/AFT model',
            fluidRow(
              box(
                width = 3,
                fileInput('tabl7_f2',label = 'Upload data'),
                selectInput('tabl7_c5','Choose t',choices = NULL),
                selectInput('tabl7_c6','Choose status',choices = NULL),
                selectInput('tabl7_c7','Choose predictor',choices = NULL),
                textInput('tabl7_t2','reference level'),
                actionButton('tabl7_a3',label = 'Submit')
              ),
              box(
                width = 9,
                verbatimTextOutput('tabl7_p4')
              )
            )
          ),
          tabPanel(
            title = 'Cox PH model',
            fluidRow(
              box(
                width = 3,
                fileInput('tabl7_f3',label = 'Upload data'),
                selectInput('tabl7_c8','Choose t',choices = NULL),
                selectInput('tabl7_c9','Choose status',choices = NULL),
                selectInput('tabl7_c10','Choose predictor',choices = NULL),
                textInput('tabl7_t3','reference level'),
                actionButton('tabl7_a4',label = 'Submit')
              ),
              box(
                width = 9,
                verbatimTextOutput('tabl7_p5')
              )
            )
          )
        )
      ),
      tabItem(
        tabName = 'tabs1',
        fluidRow(
          box(
            title = 'Tune',
            width = 3,
            fileInput('tabs1_f1',label = 'Upload Training set'),
            selectInput('tabs1_c1',label = 'Task',choices = c('classification','regression')),
            selectInput('tabs1_c2',label = 'Choose x variable',choices = NULL,multiple = T),
            selectInput('tabs1_c3',label = 'Choose y variable',choices = NULL),
            selectInput('tabs1_c4',label = 'Print',choices = c('summary','result','result_learner_param_vals','archive')),
            textInput('tabs1_t1',label = 'Regularization',value = '1,10',placeholder = 'split by comma'),
            actionButton('tabs1_a1','Submit',class = 'btn',style = 'margin-bottom:12px;')
          ),
          box(
            width = 9,
            title = 'Output',
            verbatimTextOutput('tabs1_p1')
          )
        ),
        fluidRow(
          box(
            title = 'Train & Predict',
            width = 3,
            fileInput('tabs1_f2',label = 'Upload Testing set'),
            textInput('tabs1_t2',label = 'Regularization',value = '1'),
            selectInput('tabs1_c5',label = 'Choose one object',choices = c('prob','response','confusion',
                                                                           'classif.auc','classif.acc','regr.mse',
                                                                           'regr.rmse','regr.mae','truth')),
            actionButton('tabs1_a2','Submit',class = 'btn',style = 'margin-bottom:12px;'),
            verbatimTextOutput('tabs1_p3'),
            downloadButton('tabs1_d1')
          ),
          box(
            width = 9,
            collapsible = T,
            verbatimTextOutput('tabs1_p2')
          )
        ),
        fluidRow(
          box(
            width = 6,
            plotOutput('tabs1_plot1')
          ),
          box(
            width = 6,
            plotOutput('tabs1_plot2')
          )
        )
      ),
      tabItem(
        tabName = 'tabn1',
        fluidRow(
          box(
            title = 'Training',
            width=3,
            fileInput('tabn1_f1',label = 'Upload a file'),
            selectInput('tabn1_c1',label = 'Choose x variable(s)',choices = NULL,multiple = T),
            selectInput('tabn1_c2',label = 'Choose y variable',choices = NULL),
            actionButton('tabn1_a1',label = 'Train')
          ),
          box(
            title = 'Fitted Model',
            width = 9,
            collapsible = T,
            verbatimTextOutput('tabn1_p1')
          )
        ),
        fluidRow(
          box(
            title = 'Prediction',
            width = 3,
            fileInput('tabn1_f2','Upload a file'),
            actionButton('tabn1_a2',label = 'Predict'),
            downloadButton('tabn1_d1','Download Result')
          ),
          box(
            width = 9,
            collapsible = T,
            verbatimTextOutput('tabn1_p2')
          )
        ),
        fluidRow(
          box(
            width=3,
            selectInput('tabn1_c3',label = 'Choose real y',choices = NULL),
            actionButton('tabn1_a3','Submit',class='btn',style='margin-bottom:10px'),
            br(),
            verbatimTextOutput('tabn1_p3')
          ),
          box(
            width=9,
            column(
              width=6,
              plotOutput('tabn1_plot1')
            ),
            column(
              width=6,
              plotOutput('tabn1_plot2')
            )
          )
        )
      ),
      tabItem(
        tabName = 'tabd1',
        navbarPage(
          title='Step',
          tabPanel(
            'Tune',
            fluidRow(
              box(
                width = 3,
                fileInput('tabd1_f1',label = 'Upload training data'),
                selectInput('tabd1_c1',label = 'Choose x variable(s)',choices = NULL,multiple = T),
                selectInput('tabd1_c2',label = 'Choose y variable',choices = NULL)
              ),
              box(
                div(
                  HTML("<p style='text-align:center;font-size:14px'><b>Data Preview</b></p>")
                ),
                width = 9,
                dataTableOutput('tabd1_table1')
              )
            ),
            fluidRow(
              box(
                width=3,
                p(strong('maxdepth')),
                textInput('tabd1_t1',label = 'Start',value = 2),
                textInput('tabd1_t2',label = 'End',value = 10),
                p(strong('minsplit')),
                textInput('tabd1_t3',label = 'Start',value = 2),
                textInput('tabd1_t4',label = 'End',value = 10),
                actionButton('tabd1_a1',label = 'Submit')
              ),
              box(
                width=9,
                collapsible = T,
                verbatimTextOutput('tabd1_p1')
              )
            ),
            fluidRow(
              box(
                width=3,
                selectInput('tabd1_c3',label = 'Choose one object',choices = c('result','result_learner_param_vals','archive'))
              ),
              box(
                width=9,
                verbatimTextOutput('tabd1_p2')
              )
            )
          ),
          tabPanel(
            'Train & Predict',
            fluidRow(
              box(
                width = 3,
                textInput('tabd1_t5',label = 'maxdepth',value = 5),
                textInput('tabd1_t6',label = 'minsplit',value = 5),
                actionButton('tabd1_a2',label = 'Train')
              ),
              box(
                width = 9,
                plotOutput('tabd1_plot1')
              )
            ),
            fluidRow(
              box(
                title='Prediction',
                width = 3,
                fileInput('tabd1_f2',label = 'Upload prediction data'),
                selectInput('tabd1_c4',label = 'Choose one object',choices = c('prob','response','confusion','classif.auc','classif.acc')),
                actionButton('tabd1_a3',label = 'Predict',class='btn',style='margin-bottom:10px'),
                verbatimTextOutput('tabd1_p5'),
                downloadButton('tabd1_d1')
                
              ),
              box(
                width = 9,
                collapsible = T,
                box(
                  width = 7,
                  verbatimTextOutput('tabd1_p3')
                ),
                box(
                  width = 5,
                  verbatimTextOutput('tabd1_p4')
                )
              )
            ),
            fluidRow(
              column(
                width=6,
                plotOutput('tabd1_plot2')
              ),
              column(
                width=6,
                plotOutput('tabd1_plot3')
              )
            )
          )
        )
      ),
      tabItem(
        tabName = 'tabe1',
        navbarPage(
          title='Step',
          tabPanel(
            title = 'Tune',
            fluidRow(
              box(
                width = 12,
                column(
                  width=2,
                  fileInput('tabe1_f1','Upload training data'),
                  selectInput('tabe1_c1',label = 'Choose x variable(s)',choices = NULL,multiple = T),
                  selectInput('tabe1_c2',label = 'Choose y variable',choices = NULL),
                  actionButton('tabe1_a1',label = 'Submit')
                ),
                column(
                  width=2,
                  textInput('tabe1_t1','max.depth',value = '2,5',placeholder = 'split by comma')
                ),
                column(
                  width=2,
                  textInput('tabe1_t2','min.node.size',value = '1,5',placeholder = 'split by comma')
                ),
                column(
                  width=2,
                  textInput('tabe1_t3','mtry',value = '2,5',placeholder = 'split by comma')
                ),
                column(
                  width=2,
                  textInput('tabe1_t4','num.tree',value = '500,600,20',placeholder = 'start end step')
                ),
                column(
                  width=2,
                  selectInput('tabe1_c3','type',choices = c('classification','regression'))
                )
              )
            ),
            fluidRow(
              box(
                width = 2,
                selectInput('tabe1_c4','Result',choices = c('result',"archive","result_learner_param_vals","result_x_search_space"))
              ),
              column(
                width = 6,
                verbatimTextOutput('tabe1_p1')
              ),
              column(
                width = 4,
                verbatimTextOutput('tabe1_p2')
              )
            )
          ),
          tabPanel(
            title = 'Train & Predict',
            fluidRow(
              box(
                width = 12,
                column(
                  width=2,
                  fileInput('tabe1_f2','Upload training data'),
                  selectInput('tabe1_c5',label = 'Choose x variable(s)',choices = NULL,multiple = T),
                  selectInput('tabe1_c6',label = 'Choose y variable',choices = NULL),
                  actionButton('tabe1_a2',label = 'Submit')
                ),
                column(
                  width=2,
                  textInput('tabe1_t5','max.depth',value = 3)
                ),
                column(
                  width=2,
                  textInput('tabe1_t6','min.node.size',value = 2)
                ),
                column(
                  width=2,
                  textInput('tabe1_t7','mtry',value = 3)
                ),
                column(
                  width=2,
                  textInput('tabe1_t8','num.tree',value = 540)
                ),
                column(
                  width=2,
                  selectInput('tabe1_c7','type',choices = c('classification','regression'))
                )
              )
            ),
            fluidRow(
              box(
                width = 6,
                verbatimTextOutput('tabe1_p3')
              ),
              box(
                width = 6,
                plotOutput('tabe1_plot1')
              )
            ),
            fluidRow(
              box(
                width = 2,
                fileInput('tabe1_f3','Upload testing data'),
                selectInput('tabe1_c8',label = 'Choose one object',choices = c('prob','response','confusion',
                                                                               'classif.auc','classif.acc','regr.mse',
                                                                               'regr.rmse','regr.mae','truth')),
                actionButton('tabe1_a3',label = 'Submit',class='btn',style='margin-bottom:10px;'),
                verbatimTextOutput('tabe1_p6'),
                downloadButton('tabe1_d1')
              ),
              box(
                width = 6,
                verbatimTextOutput('tabe1_p4')
              ),
              box(
                width = 4,
                collapsible = T,
                collapsed = T,
                verbatimTextOutput('tabe1_p5')
              )
            ),
            fluidRow(
              column(
                width = 6,
                plotOutput('tabe1_plot2')
              ),
              column(
                width = 6,
                plotOutput('tabe1_plot3')
              )
            )
          )
        )
      ),
      tabItem(
        tabName = 'tabe2',
        fluidRow(
          box(
            title = 'Data',
            width = 3,
            fileInput('tabe2_f1','Training set'),
            selectInput('tabe2_c1',label = 'Choose x variable(s)',choices = NULL,multiple = T),
            selectInput('tabe2_c2',label = 'Choose y variable',choices = NULL)
          ),
          box(
            width = 2,
            selectInput('tabe2_c3',label = 'Aim',choices = c('tune','fit')),
            textInput('tabe2_t1',label = 'cv',value = 5),
            textInput('tabe2_t2',label = 'maxdepth',value = 4),
            textInput('tabe2_t3',label = 'minsplit',value = 4),
            textInput('tabe2_t4',label = 'minbucket',value = 3),
            actionButton('tabe2_a1',label = 'Submit')
          ),
          box(
            title = 'Output',
            width = 7,
            collapsible = T,
            verbatimTextOutput('tabe2_p1')
          )
        ),
        fluidRow(
          box(
            width = 3,
            fileInput('tabe2_f2','Testing set'),
            selectInput('tabe2_c4',label = 'Object',choices = c('auc','error','class','prob','votes')),
            actionButton('tabe2_a2',label = 'Submit'),
            actionButton('tabe2_a3',label = 'Draw plots'),
            br(),
            br(),
            downloadButton('tabe2_d1')
          ),
          column(
            width = 9,
            verbatimTextOutput('tabe2_p2')
          )
        ),
        fluidRow(
          column(
            width = 6,
            plotOutput('tabe2_plot1')
          ),
          column(
            width = 6,
            plotOutput('tabe2_plot2')
          )
        )
      ),
      tabItem(
        tabName = 'tabe3',
        navbarPage(
          title='Step',
          tabPanel(
            title = 'tune',
            fluidRow(
              box(
                width = 12,
                column(
                  width=2,
                  fileInput('tabe3_f1','Train set'),
                  selectInput('tabe3_c1',label = 'Choose x variable(s)',choices = NULL,multiple = T),
                  selectInput('tabe3_c2',label = 'Choose y variable',choices = NULL),
                  actionButton('tabe3_a1',label = 'Submit')
                ),
                column(
                  width=2,
                  textInput('tabe3_t1','max.depth',value = '2,5',placeholder = 'start,end')
                ),
                column(
                  width=2,
                  textInput('tabe3_t2','eta',value = '0.3,0.5,0.1',placeholder = 'start,end,step')
                ),
                column(
                  width=2,
                  textInput('tabe3_t3','gamma',value = '0.3,0.5,0.1',placeholder = 'start,end,step')
                ),
                column(
                  width=2,
                  textInput('tabe3_t4','lambda',value = '1,2,1',placeholder = 'start,end,step')
                ),
                column(
                  width=2,
                  selectInput('tabe3_c3','type',choices = c('classification','regression'))
                )
              )
            ),
            fluidRow(
              box(
                width = 2,
                selectInput('tabe3_c4','Result',choices = c('result',"archive","result_learner_param_vals","result_x_search_space")),
                actionButton('tabe3_a2',label = 'Submit')
              ),
              column(
                width = 6,
                verbatimTextOutput('tabe3_p1')
              ),
              column(
                width = 4,
                verbatimTextOutput('tabe3_p2')
              )
            )
          ),
          tabPanel(
            title = 'Train & Predict',
            fluidRow(
              box(
                width = 12,
                column(
                  width=2,
                  fileInput('tabe3_f2','Upload training data'),
                  selectInput('tabe3_c5',label = 'Choose x variable(s)',choices = NULL,multiple = T),
                  selectInput('tabe3_c6',label = 'Choose y variable',choices = NULL),
                  actionButton('tabe3_a3',label = 'Submit')
                ),
                column(
                  width=2,
                  textInput('tabe3_t5','max.depth',value = 3)
                ),
                column(
                  width=2,
                  textInput('tabe3_t6','eta',value = 0.3)
                ),
                column(
                  width=2,
                  textInput('tabe3_t7','gamma',value = 0.5)
                ),
                column(
                  width=2,
                  textInput('tabe3_t8','lambda',value = 1)
                ),
                column(
                  width=2,
                  selectInput('tabe3_c7','type',choices = c('classification','regression'))
                )
              )
            ),
            fluidRow(
              box(
                width = 6,
                verbatimTextOutput('tabe3_p3')
              ),
              box(
                width = 6,
                plotOutput('tabe3_plot1')
              )
            ),
            fluidRow(
              box(
                width = 2,
                fileInput('tabe3_f3','Upload testing data'),
                selectInput('tabe3_c8',label = 'Choose one object',choices = c('prob','response','confusion',
                                                                               'classif.auc','classif.acc','regr.mse',
                                                                               'regr.rmse','regr.mae','truth')),
                actionButton('tabe3_a4',label = 'Submit',class='btn',style='margin-bottom:10px;'),
                verbatimTextOutput('tabe3_p6'),
                downloadButton('tabe3_d1')
              ),
              box(
                width = 6,
                verbatimTextOutput('tabe3_p4')
              ),
              box(
                width = 4,
                collapsible = T,
                collapsed = T,
                verbatimTextOutput('tabe3_p5')
              )
            ),
            fluidRow(
              column(
                width = 6,
                plotOutput('tabe3_plot2')
              ),
              column(
                width = 6,
                plotOutput('tabe3_plot3')
              )
            )
          )
        )
      ),
      tabItem(
        tabName = 'tabe4',
        fluidRow(
          box(
            width = 12,
            column(
              width=2,
              fileInput('tabe4_f1','Training set'),
              selectInput('tabe4_c1',label = 'Choose x variable(s)',choices = NULL,multiple = T),
              selectInput('tabe4_c2',label = 'Choose y variable',choices = NULL),
              selectInput('tabe4_c3','type',choices = c('tune','fit')),
              actionButton('tabe4_a1',label = 'Submit')
            ),
            column(
              width=2,
              textInput('tabe4_t1','max_depth',value = 5)
            ),
            column(
              width=2,
              textInput('tabe4_t2','eta',value = 1.9)
            ),
            column(
              width=2,
              textInput('tabe4_t3','feature_fraction',value = 1)
            ),
            column(
              width=2,
              textInput('tabe4_t4','lambda',value = 1)
            ),
            column(
              width=2,
              selectInput('tabe4_c4','type',choices = c('binary','regression'))
            )
          )
        ),
        fluidRow(
          box(
            width = 2,
            fileInput('tabe4_f2','Testing set'),
            actionButton('tabe4_a2',label = 'Submit'),
            downloadButton('tabe4_d1')
          ),
          box(
            width = 10,
            collapsible = T,
            verbatimTextOutput('tabe4_p1')
          )
        ),
        fluidRow(
          column(
            width = 6,
            plotOutput('tabe4_plot1')
          ),
          column(
            width = 6,
            plotOutput('tabe4_plot2')
          )
        )
      ),
      tabItem(
        tabName = 'tabf3',
        navbarPage(
          title = 'MPT',
          tabPanel(
            title = 'Portfolio with 2 assets',
            fluidRow(
              box(
                width = 12,
                column(
                  width=2,
                  textInput('tabf3_t1',label = 'mu1',value = 0.12)
                ),
                column(
                  width=2,
                  textInput('tabf3_t2',label = 'mu2',value = 0.08)
                ),
                column(
                  width=2,
                  textInput('tabf3_t3',label = 'sigma1',value = 1)
                ),
                column(
                  width=2,
                  textInput('tabf3_t4',label = 'sigma2',value = 2)
                ),
                column(
                  width=2,
                  textInput('tabf3_t5',label = 'p',value = -0.5)
                ),
                fluidRow(
                  column(
                    width = 10,
                    align="center",
                    style='margin-top:-2px',
                    actionButton('tabf3_a1',tags$div(strong('Submit')))
                  )
                )
              )
            ),
            fluidRow(
              column(
                width=4,
                verbatimTextOutput('tabf3_p1')
              ),
              column(
                width=8,
                plotOutput('tabf3_plot1')
              )
            )
          ),
          tabPanel(
            title='Portfolio with N assets',
            fluidRow(
              box(
                width = 2,
                fileInput('tabf3_f1',label = 'Upload a file'),
                selectInput('tabf3_c1',label = 'Choose stocks',choices = NULL,multiple = T),
                textInput('tabf3_t6',label = 'Target mean return'),
                actionButton('tabf3_a2','Submit',class='btn',style='margin-bottom:10px'),
                verbatimTextOutput('tabf3_p3')
              ),
              box(
                width = 3,
                verbatimTextOutput('tabf3_p2')
              ),
              box(
                width = 7,
                plotOutput('tabf3_plot2')
              )
            ),
            p('Alternatively, the short selling may not be allowed, that is, Wi>=0 for all i. 
              Then the most efficient protfolio given a target mean return is listed below.',
              style='font-size:16px'),
            fluidRow(
              column(
                width=12,
                verbatimTextOutput('tabf3_p4')
              )
            )
          ),
          tabPanel(
            title = 'Portfolio with a risk-free asset',
            p('Two risky assets with a risk-free asset',align='center',style='font-size:16px;color:blue;'),
            fluidRow(
              box(
                width = 12,
                column(
                  width=2,
                  textInput('tabf3_rf1',label = 'mu0',value = 0.06)
                ),
                column(
                  width=2,
                  textInput('tabf3_rf2',label = 'mu1',value = 0.14)
                ),
                column(
                  width=2,
                  textInput('tabf3_rf3',label = 'mu2',value = 0.08)
                ),
                column(
                  width=2,
                  textInput('tabf3_rf6',label = 'p',value = 0)
                ),
                column(
                  width=2,
                  textInput('tabf3_rf7',label = 'targetmu',value = 0.08107692)
                ),
                column(
                  width=1,
                  textInput('tabf3_rf4',label = 'sd1',value = 0.2)
                ),
                column(
                  width=1,
                  textInput('tabf3_rf5',label = 'sd2',value = 0.15)
                ),
                fluidRow(
                  column(
                    width = 12,
                    align="center",
                    style='margin-top:-2px',
                    actionButton('tabf3_rfa1',tags$div(strong('Submit')))
                  )
                )
              )
            ),
            fluidRow(
              column(
                width=4,
                verbatimTextOutput('tabf3_rfp1')
              ),
              column(
                width=8,
                plotOutput('tabf3_rfplot1')
              )
            ),
            br(),
            p('N risky assets with a risk-free asset (N>=2)',align='center',style='font-size:16px;color:blue;'),
            fluidRow(
              box(
                width=3,
                fileInput('tabf3_file1','Upload a file'),
                selectInput('tabf3_select1','Select risky assets',choices = NULL,multiple = T),
                textInput('tabf3_text1','Input mu0',value = 0.0001713433),
                textInput('tabf3_text2','Input target return',value = 0.001),
                actionButton('tabf3_action1','Submit')
              ),
              box(
                width = 9,
                verbatimTextOutput('tabf3_print1')
              )
            )
          ),
          tabPanel(
            title = 'Portfolio VaR & ES',
            fluidRow(
              box(
                title = 'Copula',
                width = 3,
                fileInput('tabf3_cpf1','Upload a file'),
                selectInput('tabf3_cpc1','Select assets',choices = NULL,multiple = T),
                actionButton('tabf3_cpa1','Submit')
              ),
              box(
                title = 'Gaussian Copula Result',
                width = 9,
                verbatimTextOutput('tabf3_cpp1')
              )
            ),
            fluidRow(
              box(
                title = 'fit T distribution',
                width = 3,
                textInput('tabf3_cpt1','Search df',value = '5,7,0.01',placeholder = 'begin,end,by'),
                actionButton('tabf3_cpa2','Submit',class='btn',style='margin-bottom:14px'),
                verbatimTextOutput('tabf3_cpp2')
              ),
              box(
                width = 9,
                fluidRow(
                  column(
                    width = 4,
                    textInput('tabf3_cpt2','alpha',value = '0.05'),
                    textInput('tabf3_cpt3','Portfolio weight',value = '0.3,0.2,0.5')
                  ),
                  column(
                    width = 8,
                    verbatimTextOutput('tabf3_cpp3')
                  )
                )
              )
            )
          )
        )
      ),
      tabItem(
        tabName = 'tabf4',
        fluidRow(
          box(
            width = 3,
            fileInput('tabf4_f1','Upload data')
          ),
          box(
            width = 2,
            selectInput('tabf4_c1','Choose stock',choices = NULL)
          ),
          box(
            width = 3,
            selectInput('tabf4_c2','Estimate type',choices = c('Nonparametric','Parametric'))
          ),
          box(
            width = 2,
            selectInput('tabf4_c3','Distribution',choices = c('Gaussian','T'))
          ),
          box(
            width = 2,
            textInput('tabf4_t1','Quantile',value = 0.05)
          )
        ),
        fluidRow(
          column(
            width = 12,
            align="center",
            style='margin-top:-25px',
            actionButton('tabf4_a1',tags$div(strong('Submit')))
          )
        ),
        br(),
        fluidRow(
          column(
            width = 12,
            verbatimTextOutput('tabf4_p1')
          )
        )
      ),
      tabItem(
        tabName = 'tabf1',
        tags$head(
          tags$style(
            HTML(
              '#tabf1_a1{color:black;background-color:skyblue}'
            )
          )
        ),
        navbarPage(
          title='CAPM',
          tabPanel(
            title = 'Classic Model',
            fluidRow(
              box(
                width = 12,
                column(
                  width=3,
                  fileInput('tabf1_f1',label = 'Upload a file')
                ),
                column(
                  width=5,
                  selectInput('tabf1_c1',label = 'Risky assets',choices = NULL,multiple = T)
                ),
                column(
                  width=2,
                  selectInput('tabf1_c2',label = 'Risk-free asset',choices = NULL)
                ),
                column(
                  width=2,
                  selectInput('tabf1_c3',label = 'Market portfolio',choices = NULL)
                ),
                fluidRow(
                  column(
                    width = 12,
                    align="center",
                    style='margin-top:-25px',
                    actionButton('tabf1_a1',tags$div(strong('Submit')))
                  )
                )
              )
            ),
            fluidRow(
              box(
                div('Data Preview',style = 'text-align:center;font-size:18px;'),
                width = 12,
                collapsible = T,
                collapsed = T,
                dataTableOutput('tabf1_table1')
              )
            ),
            fluidRow(
              box(
                title = 'Estimate by Cov',
                width = 4,
                verbatimTextOutput('tabf1_p1')
              ),
              box(
                title = 'Estimate by OLS through risk-free asset',
                width = 8,
                verbatimTextOutput('tabf1_p2')
              )
            ),
            fluidRow(
              box(
                title = 'Most under-priced stock',
                width = 4,
                verbatimTextOutput('tabf1_p3')
              ),
              box(
                title = 'Test alpha',
                width = 8,
                verbatimTextOutput('tabf1_p4')
              )
            )
          )
        )
      ),
      tabItem(
        tabName = 'tabf2',
        navbarPage(
          title='Time Series',
          tabPanel(
            title='Stationarity Test',
            fluidRow(
              box(
                width = 3,
                fileInput('tabf2_f2','Upload a file'),
                selectInput('tabf2_c3','Choose a series',choices = NULL),
                selectInput('tabf2_c4','Difference?',choices = c('Yes','No'),selected = 'No'),
                textInput('tabf2_t3','difference order',value = 1),
                actionButton('tabf2_a2','Submit')
              ),
              box(
                width = 9,
                fluidRow(
                  column(
                    width = 6,
                    plotOutput('tabf2_plot1')
                  ),
                  column(
                    width = 6,
                    plotOutput('tabf2_plot2')
                  )
                ),
                fluidRow(
                  column(
                    width=12,
                    verbatimTextOutput('tabf2_p3')
                  )
                )
              )
            )
          ),
          tabPanel(
            title='ARIMA',
            fluidRow(
              box(
                width = 3,
                fileInput('tabf2_f1','Upload training data'),
                fileInput('tabf2_f3','Upload testing data'),
                selectInput('tabf2_c1','Choose a series',choices = NULL),
                textInput('tabf2_t4','max.p',value = '4'),
                textInput('tabf2_t5','max.q',value = '4'),
                actionButton('tabf2_a3','Find best model')
              ),
              column(
                width = 9,
                verbatimTextOutput('tabf2_p4')
              )
            ),
            fluidRow(
              box(
                width = 3,
                textInput('tabf2_t1','Type order',placeholder = 'split by comma'),
                selectInput('tabf2_c2','Include mean?',choices = c('yes','no'),selected = 'yes'),
                textInput('tabf2_t2','h-step prediction',value = '5'),
                actionButton('tabf2_a1','Submit')
              ),
              box(
                width = 9,
                fluidRow(
                  column(
                    width=12,
                    verbatimTextOutput('tabf2_p1')
                  )
                ),
                fluidRow(
                  column(
                    width=12,
                    verbatimTextOutput('tabf2_p2')
                  )
                )
              )
            ),
            fluidRow(
              box(
                width = 3,
                selectInput('tabf2_c5','Diagnostic plot',
                            choices = c('residuals plot','residuals acf plot','squared residuals acf plot',
                            'residuals pacf plot','squared residuals pacf plot'),
                            selected = 'residuals plot'),
                selectInput("tabf2_c6",'Criterion',choices = c('mse','rmse','mae')),
                actionButton('tabf2_a4','Submit',class='btn',style="margin-bottom:15px"),
                verbatimTextOutput('tabf2_p6')
              ),
              column(
                width = 9,
                fluidRow(
                  column(
                    width = 7,
                    plotOutput('tabf2_plot3')
                  ),
                  column(
                    width = 5,
                    verbatimTextOutput('tabf2_p5')
                  )
                )
              )
            )
          ),
          tabPanel(
            title = 'GARCH',
            fluidRow(
              box(
                width = 3,
                fileInput('tabf2_gf1','Upload training data'),
                fileInput('tabf2_gf2','Upload testing data'),
                selectInput('tabf2_gc1','Choose a series',choices = NULL),
                textInput('tabf2_gt1','start-year-month',value = '2015,9',placeholder = 'split by comma'),
                textInput('tabf2_gt2','arma order',value = '2,0',placeholder = 'split by comma'),
                textInput('tabf2_gt3','h-step prediction',value = 10),
                actionButton('tabf2_ga1','Submit',class='btn',style='margin-bottom:10px;'),
                verbatimTextOutput('tabf2_gp3')
              ),
              box(
                width = 9,
                fluidRow(
                  column(
                    width=7,
                    verbatimTextOutput('tabf2_gp1')
                  ),
                  column(
                    width=5,
                    verbatimTextOutput('tabf2_gp2')
                  )
                )
              )
            ),
            fluidRow(
              column(
                width = 6,
                plotOutput('tabf2_gplot1')
              ),
              column(
                width = 6,
                plotOutput('tabf2_gplot2')
              )
            ),
            fluidRow(
              column(
                width = 6,
                plotOutput('tabf2_gplot3')
              ),
              column(
                width = 6,
                plotOutput('tabf2_gplot4')
              )
            )
          ),
          tabPanel(
            title = 'MAR',
            fluidRow(
              box(
                width = 3,
                fileInput('tabf2_mf1','Upload training data'),
                fileInput('tabf2_mf2','Upload testing data'),
                selectInput('tabf2_mc1','Choose a series',choices = NULL),
                textInput('tabf2_mt1','components with order',value = '2,2,3',placeholder = 'split by comma'),
                selectInput('tabf2_mc2','Diagnostic',choices = c("residuals","U_residuals","tau_residuals","BIC" )),
                actionButton('tabf2_ma1','Submit',class='btn',style='margin-bottom:10px;'),
              ),
              column(
                width = 9,
                fluidRow(
                  column(
                    width=6,
                    verbatimTextOutput('tabf2_mp1')
                  ),
                  column(
                    width=6,
                    verbatimTextOutput('tabf2_mp2')
                  )
                )
              )
            ),
            fluidRow(
              box(
                width = 2,
                textInput('tabf2_mt2','quantile',value = 0.05),
                actionButton('tabf2_ma2','Submit',class='btn',style='margin-bottom:10px;')
              ),
              column(
                width=10,
                fluidRow(
                  box(
                    width = 5,
                    title = 'One-step Prediction',
                    verbatimTextOutput('tabf2_mp3')
                  ),
                  box(
                    width = 7,
                    plotOutput('tabf2_mplot1')
                  )
                )
              )
            ),
            fluidRow(
              box(
                width = 6,
                title = 'VaR Backtesting',
                verbatimTextOutput('tabf2_mp4')
              ),
              box(
                width = 6,
                title = 'ES Backtesting',
                verbatimTextOutput('tabf2_mp5')
              )
            )
          ),
          tabPanel(
            title = 'GSMAR',
            fluidRow(
              box(
                width = 3,
                fileInput('tabf2_gsf1','Upload training data'),
                selectInput('tabf2_gsc1','Choose a series',choices = NULL),
                textInput('tabf2_gst1','components number',value = '2'),
                textInput('tabf2_gst2','order',value = '3'),
                actionButton('tabf2_gsa1','Submit',class='btn',style='margin-bottom:10px;'),
              ),
              box(
                width = 9,
                title = 'Quantile Residual plots',
                plotOutput('tabf2_gsplot1')
              )
            ),
            fluidRow(
              box(
                width = 6,
                title = 'Summary',
                verbatimTextOutput('tabf2_gsp1')
              ),
              box(
                width = 6,
                title = 'Diagnostic',
                verbatimTextOutput('tabf2_gsp2')
              )
            ),
            fluidRow(
              box(
                width = 2,
                fileInput('tabf2_gsf2','Upload testing data'),
                textInput('tabf2_gst3','quantile',value = 0.05),
                actionButton('tabf2_gsa2','Submit',class='btn',style='margin-bottom:10px;')
              ),
              column(
                width=10,
                fluidRow(
                  box(
                    width = 5,
                    title = 'One-step Prediction',
                    verbatimTextOutput('tabf2_gsp3')
                  ),
                  box(
                    width = 7,
                    plotOutput('tabf2_gsplot2')
                  )
                )
              )
            ),
            fluidRow(
              box(
                width = 6,
                title = 'VaR Backtesting',
                verbatimTextOutput('tabf2_gsp4')
              ),
              box(
                width = 6,
                title = 'ES Backtesting',
                verbatimTextOutput('tabf2_gsp5')
              )
            )
          )
        )
      )
    )
  )
)
