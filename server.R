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

source('diy.R')
source('dda.R')

server<-function(input,output,session){
  
  # Elastic Net Regression
  
  da_lr<-reactive({
    req(input$tabl_f1)
    read_csv(input$tabl_f1$datapath)
  })
  observe({
    req(da_lr())
    updateSelectInput(session,'tabl1_c1',choices = names(da_lr()),selected = names(da_lr())[-length(names(da_lr()))])
    updateSelectInput(session,'tabl1_c2',choices = names(da_lr()),selected = names(da_lr())[length(names(da_lr()))])
  })
  glmnet1=eventReactive(
    input$tabl1_a1,
    {
      req(da_lr())
      train=da_lr()
      x=as.matrix(train[input$tabl1_c1])
      y=as.matrix(train[input$tabl1_c2])
      alpha=as.numeric(input$tabl1_t1)
      from=as.numeric(input$tabl1_t2)
      to=as.numeric(input$tabl1_t3)
      if(input$tabl1_t5==''){
        model=cv.glmnet(x,y,alpha = alpha,family='gaussian',lambda = seq(from,to,length.out=as.numeric(input$tabl1_t4)))
        model
      }
      else{
        set.seed(as.numeric(input$tabl1_t5))
        model=cv.glmnet(x,y,alpha = alpha,family='gaussian',lambda = seq(from,to,length.out=as.numeric(input$tabl1_t4)))
        model
      }
    }
  )
  observeEvent(
    input$tabl1_a1,
    {
      output$tabl1_print1<-renderPrint({
        glmnet1()[[input$tabl1_c3]]
      })
      res=c(list(glmnet1()),list(coef(glmnet1(),s=input$tabl1_c4)))
      names(res)=c('Summary','Optimal.Parameter')
      output$tabl1_print2=renderPrint({
        res
      })

    }
  )
  
  glmnet2=eventReactive(
    input$tabl1_a2,
    {
      req(da_lr())
      train=da_lr()
      x=as.matrix(train[input$tabl1_c1])
      y=as.matrix(train[input$tabl1_c2])
      alpha=as.numeric(input$tabl1_t1)
      glmnet(x,y,alpha = alpha,family='gaussian',lambda=as.numeric(input$tabl1_t7))
    }
  )
  
  da_lr1<-reactive({
    req(input$tabl1_f1)
    read_csv(input$tabl1_f1$datapath)
  })
  observeEvent(
    input$tabl1_a2,
    {
      test=da_lr1()
      xtest=as.matrix(test[input$tabl1_c1])
      output$tabl1_print3=renderPrint({
        predict(glmnet2(),xtest)
      })
      tryCatch(
        {
          if(input$tabl1_t6==''){
            output$tabl1_print4=renderPrint({
              'No real y, mse is not available'
            })
          }
          else{
            pred=predict(glmnet2(),xtest)
            y=as.matrix(test[input$tabl1_t6])
            mse=mean((pred-y)^2)
            output$tabl1_print4=renderPrint({
              cat('MSE is: ',mse)
            })
            test$pred = pred
            output$tabl1_d1=downloadHandler(
              'glmnet.csv',
              content = function(file){
                write.csv(test,file,row.names = F)
              }
            )
          }
        },error=function(e){
          output$tabl1_print4=renderPrint({
            'An error occurred, please check your data!'
          })
        }
      )
    }
  )
  
  # quantile regression
  
  observe({
    req(da_lr())
    updateSelectInput(session,'tabl3_c1',choices = names(da_lr()),selected = names(da_lr())[-length(names(da_lr()))])
    updateSelectInput(session,'tabl3_c2',choices = names(da_lr()),selected = names(da_lr())[length(names(da_lr()))])
  })
  
  qr_model<-eventReactive(
    input$tabl3_a1,
    {
      tryCatch(
        {
          tau=as.numeric(unlist(strsplit(input$tabl3_t1,',')))
          model=rq(as.formula(paste(input$tabl3_c2,'~',paste(input$tabl3_c1,collapse = '+'))),data=da_lr(),tau = tau)
          model
        },error=function(e){
          output$tabl3_p1=renderPrint({
            'tau must between 0 and 1.'
          })
        }
      )
    }
  )
  observeEvent(
    input$tabl3_a1,
    {
      req(qr_model())
      output$tabl3_p1=renderPrint({
        summary(qr_model())
      })
    }
  )
  
  da_qr<-reactive({
    req(input$tabl3_f1)
    read_csv(input$tabl3_f1$datapath)
  })
  
  observeEvent(
    input$tabl3_a2,
    {
      tryCatch(
        {
          req(qr_model())
          test=as.data.frame(da_qr())
          res=predict(qr_model(),test)
          output$tabl3_p2=renderPrint({
            res
          })
          ypred=as.data.frame(res)
          tau=unlist(strsplit(input$tabl3_t1,','))
          y=test[[input$tabl3_c2]]
          result=mse_qr(tau,y,ypred)
          test$pred = res
          output$tabl3_p3=renderPrint({
            result
          })
          output$tabl3_d1=downloadHandler(
            'qr.csv',
            content = function(file){
              write.csv(test,file,row.names = F)
            }
          )
        },error=function(e){
          output$tabl3_p2=renderPrint({
            'An error occurred, please check your data!'
          })
        }
      )
      
    }
  )
  
  
  # PCR
  da_pcr<-reactive({
    req(input$tabl4_f1)
    read_csv(input$tabl4_f1$datapath)
  })
  observe({
    req(da_pcr())
    updateSelectInput(session,'tabl4_c1',choices = names(da_pcr()),selected = names(da_pcr())[-length(names(da_pcr()))])
    updateSelectInput(session,'tabl4_c2',choices = names(da_pcr()),selected = names(da_pcr())[length(names(da_pcr()))])
  })
  pca = eventReactive(
    input$tabl4_a1,
    {
      x=da_pcr()[input$tabl4_c1]
      fit = prcomp(x,center = T,scale. = T)
      return(fit)
    }
  )
  observeEvent(
    input$tabl4_a1,
    {
      fit = pca()
      res = c(list(summary(fit)),list(fit$rotation))
      names(res) = c('summary','rotation')
      output$tabl4_p1<-renderPrint({
        res
      })
    }
  )
  pcr = eventReactive(
    input$tabl4_a2,
    {
      fit = pca()
      n=as.numeric(input$tabl4_t1)
      x=fit$x[,1:n]
      y=as.matrix(da_pcr()[input$tabl4_c2])
      model=lm(y~x)
      return(model)
    }
  )
  observeEvent(
    input$tabl4_a2,
    {
      output$tabl4_p2=renderPrint({
        summary(pcr())
      })
    }
  )
  da_pcr1<-reactive({
    req(input$tabl4_f2)
    read_csv(input$tabl4_f2$datapath)
  })
  observeEvent(
    input$tabl4_a3,
    {
      newx=da_pcr1()[input$tabl4_c1]
      x=da_pcr()[input$tabl4_c1]
      sd=apply(x,2,sd)
      me=apply(x,2,mean)
      newsx=scale(newx,center = me,scale = sd)
      fit = pca()
      n=as.numeric(input$tabl4_t1)
      newax=newsx%*%fit$rotation[,1:n]
      newax=cbind(1,newax)
      beta=as.matrix(coef(pcr()))
      output$tabl4_p3<-renderPrint({
        newax%*%beta
      })
      y=da_pcr1()[[input$tabl4_c2]]
      ypred=as.vector(newax%*%beta)
      test = as.data.frame(da_pcr1())
      test$pred = ypred
      output$tabl4_p4<-renderPrint({
        rerror(y,ypred,type = 'rmse')
      })
      output$tabl4_d1=downloadHandler(
        'pcr.csv',
        content = function(file){
          write.csv(test,file,row.names = F)
        }
      )
    }
  )
  # LDA
  da_lda1<-reactive({
    req(input$tabl5_f1)
    read_csv(input$tabl5_f1$datapath)
  })
  observe({
    req(da_lda1())
    updateSelectInput(session,'tabl5_c1',choices = names(da_lda1()),selected = names(da_lda1())[-length(names(da_lda1()))])
    updateSelectInput(session,'tabl5_c2',choices = names(da_lda1()),selected = names(da_lda1())[length(names(da_lda1()))])
  })
  lda_model<-eventReactive(
    input$tabl5_a1,
    {
      lda(as.formula(paste(input$tabl5_c2,'~',paste(input$tabl5_c1,collapse = '+'))),data=da_lda1())
    }
  )
  output$tabl5_p1=renderPrint({
    req(lda_model())
    lda_model()
  })
  da_lda2<-reactive({
    req(input$tabl5_f2)
    read_csv(input$tabl5_f2$datapath)
  })
  observeEvent(
    input$tabl5_a2,
    {
      req(lda_model(),da_lda2(),input$tabl5_c1)
      pred_obj=predict(lda_model(),da_lda2())
      da=as.data.frame(da_lda2()[input$tabl5_c1])
      da$y_pred=pred_obj$class
      da$prob=pred_obj$posterior[,2]
      output$tabl5_p2<-renderPrint({
        pred_obj
      })
      output$tabl5_d1<-downloadHandler(
        'lda.csv',
        content = function(file){
          write.csv(da,file,row.names = F)
        }
      )
    }
  )
  observe({
    req(da_lda2())
    updateSelectInput(session,'tabl5_c3',choices = names(da_lda2()),selected = names(da_lda2())[length(names(da_lda2()))])
  })
  observeEvent(
    input$tabl5_a3,
    {
      pred_obj=predict(lda_model(),da_lda2())
      prob=pred_obj$posterior[,2]
      da=data.frame(da_lda2()[input$tabl5_c3],pred_obj$class,prob)
      output$tabl5_plot1<-renderPlot({
        roc.plot(da)
      })
    }
  )
  observeEvent(
    input$tabl5_a3,
    {
      pred_obj=predict(lda_model(),da_lda2())
      prob=pred_obj$posterior[,2]
      da=data.frame(da_lda2()[input$tabl5_c3],pred_obj$class,prob)
      names(da)=c('y','y_pred,','prob')
      roc_obj1=roc(da$y,da$prob)
      output$tabl5_plot2<-renderPlot({
        ks.plot(da)
      })
      acc_score=sum(da$y_pred==da$y)/length(da[,1])
      output$tabl5_p3=renderPrint({
        cat('Accuracy score is:','\n',acc_score,'\n',
            'DIY AUC by probability approach is:','\n',diy_auc(da$y,da$prob),'\n',
            'AUC computed by "pROC":','\n',roc_obj1$auc[1],
            sep = '')
      })
      
    }
  )
  #DDA
  observeEvent(
    input$tabl5_a4,
    {
      req(da_lda1(),da_lda2(),input$tabl5_c1,input$tabl5_c2)
      x=as.data.frame(da_lda1()[input$tabl5_c1])
      y=as.data.frame(da_lda1()[input$tabl5_c2])
      xp=as.data.frame(da_lda2()[input$tabl5_c1])
      output$tabl5_p5<-renderPrint({
        dda.p(x,y,xp)
      })
      if(input$tabl5_text1!=''){
        tryCatch({
          res=dda.p(x,y,xp)
          acc_score=sum(res$pred_y==da_lda2()[input$tabl5_text1])/length(res$pred_y)
          output$tabl5_p4<-renderPrint({
            cat('Accuracy score is:',acc_score)
          })
        },error=function(e){
          output$tabl5_p4<-renderPrint({
            "y is wrong!"
          })
        })
        
      }
      else{
        output$tabl5_p4<-renderPrint({
          "No real y, accuracy score cannot be computed."
        })
      }
    }
  )
  
  # Nonparametric Regression
  da_npr1<-reactive({
    req(input$tabl6_f1)
    read_csv(input$tabl6_f1$datapath)
  })
  da_npr2<-reactive({
    req(input$tabl6_f2)
    read_csv(input$tabl6_f2$datapath)
  })
  
  observe({
    req(da_npr1())
    updateSelectInput(session,'tabl6_c1',choices = names(da_npr1()))
    updateSelectInput(session,'tabl6_c2',choices = names(da_npr1()))
  })
  observeEvent(
    input$tabl6_a1,
    {
      tryCatch(
        {
          x = da_npr1()[[input$tabl6_c1]]
          y = da_npr1()[[input$tabl6_c2]]
          newx = da_npr2()[[input$tabl6_c1]]
          h = as.numeric(input$tabl6_t1)
          if(input$tabl6_c3=='local average'){
            ypred = local_average(x,y,h,newx)
            output$tabl6_p2<-renderPrint({
              ypred
            })
          }
          else if(input$tabl6_c3=='Nadaraya-Watson'){
            ypred = NW(x,y,h,newx)
            output$tabl6_p2<-renderPrint({
              ypred
            })
          }
          else{
            p = as.numeric(input$tabl6_t2)
            ypred = local_poly(x,y,h,p,newx)
            output$tabl6_p2<-renderPrint({
              ypred
            })
          }
        },error = function(e){
          output$tabl6_p2<-renderPrint({
            'An error occurred, please check your input!'
          })
        }
      )
      tryCatch(
        {
          ytest = da_npr2()[[input$tabl6_c2]]
          output$tabl6_p1=renderPrint({
            cat('RMSE is:',rerror(ytest,ypred,type = 'rmse'))
          })
        },error=function(e){
          output$tabl6_p1=renderPrint({
            'No real y, rmse cannot be computed.'
          })
        }
      )
    }
  )
  
  #Survival Analysis
  da_sa1<-reactive({
    req(input$tabl7_f1)
    read_csv(input$tabl7_f1$datapath)
  })
  observe({
    req(da_sa1())
    updateSelectInput(session,'tabl7_c1',choices = names(da_sa1()))
    updateSelectInput(session,'tabl7_c2',choices = names(da_sa1()))
    updateSelectInput(session,'tabl7_c3',choices = names(da_sa1()))
  })
  observeEvent(
    input$tabl7_a1,
    {
      if(is.null(input$tabl7_f1)){
        t = rweibull(1000,shape = 5,scale = 3)
        fit = survreg(Surv(t)~1, dist = 'weibull')
        a = 1/fit$scale
        b = as.vector(exp(fit$coef))
        res = c(list(a),list(b))
        names(res) = c('shape.parameter','scale.parameter')
        output$tabl7_p1 = renderPrint({
          res
        })
      }
      else{
        t = da_sa1()[[input$tabl7_c1]]
        fit = survreg(Surv(t)~1, dist = 'weibull')
        a = 1/fit$scale
        b = as.vector(exp(fit$coef))
        res = c(list(a),list(b))
        names(res) = c('shape.parameter','scale.parameter')
        output$tabl7_p1 = renderPrint({
          res
        })
      }
    }
  )
  observeEvent(
    input$tabl7_a2,
    {
      req(input$tabl7_t1)
      data = da_sa1()
      fit = survfit(Surv(data[[input$tabl7_c2]],data[[input$tabl7_c3]])~1)
      output$tabl7_p2 = renderPrint({
        fit[[input$tabl7_c4]]
      })
      t = as.numeric(unlist(strsplit(input$tabl7_t1,',')))
      output$tabl7_p3 = renderPrint({
        survi(fit,t)
      })
      output$tabl7_plot1 = renderPlot({
        plot(fit,main = 'Survival function')
      })
    }
  )
  
  da_sa2<-reactive({
    req(input$tabl7_f2)
    read_csv(input$tabl7_f2$datapath)
  })
  observe({
    req(da_sa2())
    updateSelectInput(session,'tabl7_c5',choices = names(da_sa2()))
    updateSelectInput(session,'tabl7_c6',choices = names(da_sa2()))
    updateSelectInput(session,'tabl7_c7',choices = names(da_sa2()))
  })
  observeEvent(
    input$tabl7_a3,
    {
      data = da_sa2()
      data[[input$tabl7_c7]] = factor(data[[input$tabl7_c7]])
      data[[input$tabl7_c7]] <- relevel(data[[input$tabl7_c7]], input$tabl7_t2)
      time = data[[input$tabl7_c5]]
      cens = data[[input$tabl7_c6]]
      predictor = data[[input$tabl7_c7]]
      fit <- survreg(Surv(time, cens) ~ predictor)
      res1 = summary(fit)
      alpha = 1/res1$scale
      beta = res1$coefficients[2]
      hr = as.vector(exp(alpha*beta))
      lambda0 = exp(-res1$coefficients[1])
      lambda1 = exp(-sum(res1$coefficients))
      distri = matrix(0,nrow = 2, ncol = 2)
      distri[1,] = c(alpha,1/lambda0)
      distri[2,] = c(alpha,1/lambda1)
      rownames(distri) = c('reference.level','experient.level')
      colnames(distri) = c('a','b')
      phi = as.vector(exp(-beta))
      gamma = as.vector(-alpha*beta)
      result = c(list(hr),list(distri),list(phi),list(gamma),list(res1))
      names(result) = c('hazard.ratio','weibull.distribution','phi','gamma','summary')
      output$tabl7_p4=renderPrint({
        result
      })
    }
  )
  
  da_sa3<-reactive({
    req(input$tabl7_f3)
    read_csv(input$tabl7_f3$datapath)
  })
  observe({
    req(da_sa3())
    updateSelectInput(session,'tabl7_c8',choices = names(da_sa3()))
    updateSelectInput(session,'tabl7_c9',choices = names(da_sa3()))
    updateSelectInput(session,'tabl7_c10',choices = names(da_sa3()))
  })
  observeEvent(
    input$tabl7_a4,
    {
      data = da_sa3()
      data[[input$tabl7_c10]] = factor(data[[input$tabl7_c10]])
      data[[input$tabl7_c10]] <- relevel(data[[input$tabl7_c10]], input$tabl7_t3)
      time = data[[input$tabl7_c8]]
      cens = data[[input$tabl7_c9]]
      predictor = data[[input$tabl7_c10]]
      fit = coxph(Surv(time,cens)~predictor,method = 'exact')
      output$tabl7_p5=renderPrint({
        summary(fit)
      })
    }
  )
  
  # SVM
  
  da_svm1<-reactive({
    req(input$tabs1_f1)
    read_csv(input$tabs1_f1$datapath)
  })
  da_svm2<-reactive({
    req(input$tabs1_f2)
    read_csv(input$tabs1_f2$datapath)
  })
  observe({
    req(da_svm1())
    updateSelectInput(session,'tabs1_c2',choices = names(da_svm1()),selected = names(da_svm1())[-length(names(da_svm1()))])
    updateSelectInput(session,'tabs1_c3',choices = names(da_svm1()),selected = names(da_svm1())[length(names(da_svm1()))])
  })
  observeEvent(
    input$tabs1_a1,
    {
      if(input$tabs1_c1=='classification'){
        train=as.data.frame(da_svm1())
        cost = as.numeric(unlist(strsplit(input$tabs1_t1,',')))
        task = as_task_classif(train,target = input$tabs1_c3)
        learner = lrn("classif.svm", type = "C-classification", kernel = "linear",
                      cost = to_tune(cost[1],cost[2]),
                      predict_type='prob')
        
        instance = mlr3tuning::tune(
          tuner = tnr("grid_search"),
          task = task,
          learner = learner,
          resampling = rsmp("cv", folds = 10),
          measure = msr("classif.auc"))
        if(input$tabs1_c4=='summary'){
          output$tabs1_p1 = renderPrint({
            instance
          })
        }
        else{
          output$tabs1_p1 = renderPrint({
            instance[[input$tabs1_c4]]
          })
        }
      }
      else{
        train=as.data.frame(da_svm1())
        cost = as.numeric(unlist(strsplit(input$tabs1_t1,',')))
        task = as_task_regr(train,target = input$tabs1_c3)
        learner = lrn("regr.svm", type = "eps-regression", kernel = "linear",
                      cost = to_tune(cost[1],cost[2]))
        instance = mlr3tuning::tune(
          tuner = tnr("grid_search"),
          task = task,
          learner = learner,
          resampling = rsmp("cv", folds = 10),
          measure = msr("regr.mse"))
        if(input$tabs1_c4=='summary'){
          output$tabs1_p1 = renderPrint({
            instance
          })
        }
        else{
          output$tabs1_p1 = renderPrint({
            instance[[input$tabs1_c4]]
          })
        }
      }
    }
  )
  svm1 = eventReactive(
    input$tabs1_a2,
    {
      if(input$tabs1_c1=='classification'){
        train=as.data.frame(da_svm1())
        task = as_task_classif(train,target = input$tabs1_c3)
        learner = lrn("classif.svm", type = "C-classification", kernel = "linear",
                      cost = as.numeric(input$tabs1_t2),
                      predict_type='prob')
        learner$train(task)
      }
      else{
        train=as.data.frame(da_svm1())
        task = as_task_regr(train,target = input$tabs1_c3)
        learner = lrn("regr.svm", type = "eps-regression", kernel = "linear",
                      cost = as.numeric(input$tabs1_t2))
        learner$train(task)
      }
    }
  )
  observeEvent(
    input$tabs1_a2,
    {
      tryCatch(
        {
          if(input$tabs1_c1=='classification'){
            test = as.data.frame(da_svm2())
            test[[input$tabs1_c3]]=factor(test[[input$tabs1_c3]])
            res = svm1()$predict_newdata(test)
            if(input$tabs1_c5%in%c('classif.acc','classif.auc')){
              output$tabs1_p2<-renderPrint({
                print(res$score(msr(input$tabs1_c5)))
              })
            }
            else if(input$tabs1_c5%in%c('prob','response','confusion')){
              output$tabs1_p2<-renderPrint({
                print(res[[input$tabs1_c5]])
              })
            }
            da=data.frame(da_svm2()[input$tabs1_c3],res$response,res$prob[,2])
            output$tabs1_plot2<-renderPlot({
              ks.plot(da)
            })
            output$tabs1_plot1<-renderPlot({
              roc.plot(da)
            })
            test1 = test[c(input$tabs1_c2,input$tabs1_c3)]
            test1$pred = res$response
            output$tabs1_d1=downloadHandler(
              'svm.csv',
              content = function(file){
                write.csv(test1,file,row.names = F)
              }
            )
          }
          else{
            test = as.data.frame(da_svm2())
            res = svm1()$predict_newdata(test)
            if(input$tabs1_c5%in%c('regr.mse','regr.mae','regr.rmse')){
              output$tabs1_p2<-renderPrint({
                print(res$score(msr(input$tabs1_c5)))
              })
            }
            else if(input$tabs1_c5%in%c('response','truth')){
              output$tabs1_p2<-renderPrint({
                print(res[[input$tabs1_c5]])
              })
            }
            ypred = res$response
            y = test[[input$tabs1_c3]]
            output$tabs1_p3 = renderPrint({
              cat('RMSE is:',rerror(y,ypred,type = 'rmse'))
            })
            test1 = test[c(input$tabs1_c2,input$tabs1_c3)]
            test1$pred = res$response
            output$tabs1_d1=downloadHandler(
              'svm.csv',
              content = function(file){
                write.csv(test1,file,row.names = F)
              }
            )
          }
        },error = function(e){
          output$tabs1_p2<-renderPrint({
            'An error occurred, please check your input!'
          })
        }
      )
    }
  )
  
  
  # Naive Bayes Classfier
  da_bayes1<-reactive({
    req(input$tabn1_f1)
    read_csv(input$tabn1_f1$datapath)
  })
  observe({
    req(da_bayes1())
    updateSelectInput(session,'tabn1_c1',choices = names(da_bayes1()),selected = names(da_bayes1())[-length(names(da_bayes1()))])
    updateSelectInput(session,'tabn1_c2',choices = names(da_bayes1()),selected = names(da_bayes1())[length(names(da_bayes1()))])
  })
  da_bayes2<-reactive({
    req(da_bayes1())
    data=as.data.frame(da_bayes1())
    logic=sapply(data,is.numeric)
    data[!logic]=lapply(data[!logic],factor)
    return(data)
  })
  nb_model<-eventReactive(
    input$tabn1_a1,
    {
      naiveBayes(as.formula(paste(input$tabn1_c2,'~',paste(input$tabn1_c1,collapse = '+'))),data=da_bayes2(),laplace=1)
    }
  )
  output$tabn1_p1=renderPrint({
    req(nb_model())
    nb_model()
  })
  da_bayes3<-reactive({
    req(input$tabn1_f2)
    read_csv(input$tabn1_f2$datapath)
  })
  da_bayes4<-reactive({
    req(da_bayes3())
    data=as.data.frame(da_bayes3())
    logic=sapply(data,is.numeric)
    data[!logic]=lapply(data[!logic],factor)
    return(data)
  })
  observeEvent(
    input$tabn1_a2,
    {
      req(nb_model(),da_bayes4(),input$tabn1_c1)
      pred_y=predict(nb_model(),da_bayes4())
      pred_obj2=predict(nb_model(),da_bayes4(),type = 'raw')
      res=list(pred_y,pred_obj2)
      names(res)=c('Class','Posterior Probability')
      output$tabn1_p2<-renderPrint({
        print(res)
      })
      output$tabn1_d1<-downloadHandler(
        'nb.csv',
        content = function(file){
          da=cbind(da_bayes3()[input$tabn1_c1],pred_y)
          write.csv(da,file,row.names = F)
        }
      )
    }
  )
  observe({
    req(da_bayes3())
    updateSelectInput(session,'tabn1_c3',choices = names(da_bayes3()),selected = names(da_bayes3())[length(names(da_bayes3()))])
  })
  observeEvent(
    input$tabn1_a3,
    {
      req(nb_model(),da_bayes4(),input$tabn1_c3,da_bayes3())
      tryCatch(
        {
          pred_obj1=predict(nb_model(),da_bayes4())
          pred_obj2=predict(nb_model(),da_bayes4(),type = 'raw')[,2]
          da=data.frame(da_bayes3()[input$tabn1_c3],pred_obj1,pred_obj2)
          names(da)=c('y','y_pred,','prob')
          roc_obj1=roc(da$y,da$prob)
          output$tabn1_plot2<-renderPlot({
            ks.plot(da)
          })
          acc_score=sum(da$y_pred==da$y)/length(da[,1])
          output$tabn1_p3=renderPrint({
            cat('Accuracy score is:','\n',acc_score,'\n',
                'DIY AUC by probability approach is:','\n',diy_auc(da$y,da$prob),'\n',
                'AUC computed by "pROC":','\n',roc_obj1$auc[1],
                sep = '')
          })
          output$tabn1_plot1<-renderPlot({
            roc.plot(da)
          })
        },error=function(e){
          output$tabn1_p3=renderPrint({
            'An error occurred, please try again!'
          })
        }
      )
    }
  )
  
  
  # Desicion Tree
  da_dt1<-reactive({
    req(input$tabd1_f1)
    read_csv(input$tabd1_f1$datapath)
  })
  observe({
    req(da_dt1())
    updateSelectInput(session,'tabd1_c1',choices = names(da_dt1()),selected = names(da_dt1())[-length(names(da_dt1()))])
    updateSelectInput(session,'tabd1_c2',choices = names(da_dt1()),selected = names(da_dt1())[length(names(da_dt1()))])
  })
  output$tabd1_table1<-renderDT(
    datatable(
      da_dt1(),
      options = list(
        columnDefs=list(list(className='dt-center',targets=1:length(colnames(da_dt1())))),
        scrollX=T,
        lengthMenu=c(10,20,50)
      )
    )
  )
  da_dt2<-reactive({
    req(input$tabd1_f1,input$tabd1_c1,input$tabd1_c2,da_dt1())
    cbind(da_dt1()[input$tabd1_c1],da_dt1()[input$tabd1_c2])
  })
  DTtask<-eventReactive(
    input$tabd1_a1,
    {
      train=as.data.frame(da_dt2())
      task = as_task_classif(train,target = input$tabd1_c2)
      learner <- lrn("classif.rpart",maxdepth=to_tune(as.numeric(input$tabd1_t1),as.numeric(input$tabd1_t2)),
                     minsplit=to_tune(as.numeric(input$tabd1_t3),as.numeric(input$tabd1_t4)),predict_type='prob')
      instance = tune(
        tuner = tnr("grid_search"),
        task = task,
        learner = learner,
        resampling = rsmp("cv", folds = 10),
        measure = msr("classif.auc")) 
      return(instance)
    }
  )
  observeEvent(
    input$tabd1_a1,
    {
      output$tabd1_p1=renderPrint({
        print(DTtask())
      })
    }
  )
  observeEvent(
    input$tabd1_a1,
    {
      output$tabd1_p2=renderPrint({
        DTtask()[[input$tabd1_c3]]
      })
    }
  )
  dt1=eventReactive(
    input$tabd1_a2,
    {
      train=as.data.table(da_dt2())
      task = as_task_classif(train,target = input$tabd1_c2)
      learner=lrn("classif.rpart",maxdepth=as.numeric(input$tabd1_t5),
                   minsplit=as.numeric(input$tabd1_t6),predict_type='prob')
      learner$train(task)
    }
  )
  observeEvent(
    input$tabd1_a2,
    {
      output$tabd1_plot1=renderPlot({
        rpart.plot(dt1()$model,roundint = F)
      })
    }
  )
  da_dt3<-reactive({
    req(input$tabd1_f2)
    read_csv(input$tabd1_f2$datapath)
  })
  da_dt4<-reactive({
    req(da_dt3())
    tryCatch(
      {
        data=as.data.frame(da_dt3())
        data[[input$tabd1_c2]]=factor(data[[input$tabd1_c2]])
        #data=da_dt3()%>%select(-input$tabd1_c2)
        return(data)
      },error=function(e){
        return(da_dt3())
      }
    )
  })
  observeEvent(
    input$tabd1_a3,
    {
      res=dt1()$predict_newdata(da_dt4())
      output$tabd1_p3<-renderPrint({
        print(res)
      })
      if(input$tabd1_c4%in%c('classif.acc','classif.auc')){
        tryCatch(
          {
            output$tabd1_p4<-renderPrint({
              print(res$score(msr(input$tabd1_c4)))
            })
          },error=function(e){
            output$tabd1_p4<-renderPrint({
              print('An error occurred since there is no real y.')
            })
          }
        )
      }
      else{
        output$tabd1_p4<-renderPrint({
          print(res[[input$tabd1_c4]])
        })
      }
    }
  )
  
  observeEvent(
    input$tabd1_a3,
    {
      req(da_dt4(),dt1(),input$tabd1_c2)
      tryCatch(
        {
          res=dt1()$predict_newdata(da_dt4())
          da=data.frame(da_dt4()[input$tabd1_c2],res$response,res$prob[,2])
          output$tabd1_plot3<-renderPlot({
            ks.plot(da)
          })
          output$tabd1_plot2<-renderPlot({
            roc.plot(da)
          })
          test = as.data.frame(da_dt3())
          test1 = test[c(input$tabd1_c1,input$tabd1_c2)]
          test1$pred = res$response
          output$tabd1_d1=downloadHandler(
            'dt.csv',
            content = function(file){
              write.csv(test1,file,row.names = F)
            }
          )
          
        },error=function(e){
          output$tabd1_p5=renderPrint({
            'An error occurred since there is no real y.'
          })
        }
      )
    }
  )
  
  
  # RF
  
  da_rf1<-reactive({
    req(input$tabe1_f1)
    read_csv(input$tabe1_f1$datapath)
  })
  observe({
    req(da_rf1())
    updateSelectInput(session,'tabe1_c1',choices = names(da_rf1()),selected = names(da_rf1())[-length(names(da_rf1()))])
    updateSelectInput(session,'tabe1_c2',choices = names(da_rf1()),selected = names(da_rf1())[length(names(da_rf1()))])
  })
  observeEvent(
    input$tabe1_a1,
    {
      data=as.data.frame(da_rf1())
      train=cbind(data[input$tabe1_c1],data[input$tabe1_c2])
      max_depth=as.numeric(unlist(strsplit(input$tabe1_t1,',')))
      min_node_size=as.numeric(unlist(strsplit(input$tabe1_t2,',')))
      mtry=as.numeric(unlist(strsplit(input$tabe1_t3,',')))
      num_tree=as.numeric(unlist(strsplit(input$tabe1_t4,',')))
      ntrees=seq(num_tree[1],num_tree[2],num_tree[3])
      if(input$tabe1_c3=='classification'){
        task = as_task_classif(train,target = input$tabe1_c2)
        learner = lrn('classif.ranger',max.depth=to_tune(max_depth[1],max_depth[2]),
                      min.node.size=to_tune(min_node_size[1],min_node_size[2]),
                      mtry=to_tune(mtry[1],mtry[2]),
                      num.trees=to_tune(ntrees),
                      predict_type='prob')
        instance = tune(
          tuner = tnr("grid_search"),
          task = task,
          learner = learner,
          resampling = rsmp("cv", folds = 10),
          measure = msr("classif.auc"))
      }
      else{
        ntrees=seq(num_tree[1],num_tree[2],num_tree[3])
        task = as_task_regr(train,target = input$tabe1_c2)
        learner = lrn('regr.ranger',max.depth=to_tune(max_depth[1],max_depth[2]),
                      min.node.size=to_tune(min_node_size[1],min_node_size[2]),
                      mtry=to_tune(mtry[1],mtry[2]),
                      num.trees=to_tune(ntrees))
        instance = tune(
          tuner = tnr("grid_search"),
          task = task,
          learner = learner,
          resampling = rsmp("cv", folds = 10),
          measure = msr("regr.mse"))
      }
      output$tabe1_p1<-renderPrint({
        instance
      })
      output$tabe1_p2<-renderPrint({
        instance[[input$tabe1_c4]]
      })
    }
  )
  da_rf2<-reactive({
    req(input$tabe1_f2)
    read_csv(input$tabe1_f2$datapath)
  })
  observe({
    req(da_rf2())
    updateSelectInput(session,'tabe1_c5',choices = names(da_rf2()),selected = names(da_rf2())[-length(names(da_rf2()))])
    updateSelectInput(session,'tabe1_c6',choices = names(da_rf2()),selected = names(da_rf2())[length(names(da_rf2()))])
  })
  rf=eventReactive(
    input$tabe1_a2,
    {
      if(input$tabe1_c7=='classification'){
        data=as.data.frame(da_rf2())
        train=cbind(data[input$tabe1_c5],data[input$tabe1_c6])
        max_depth=as.numeric(input$tabe1_t5)
        min_node_size=as.numeric(input$tabe1_t6)
        mtry=as.numeric(input$tabe1_t7)
        num_tree=as.numeric(input$tabe1_t8)
        task = as_task_classif(train,target = input$tabe1_c6)
        learner = lrn('classif.ranger',max.depth=max_depth,min.node.size=min_node_size,
                      mtry=mtry,predict_type='prob',importance = "impurity",num.trees=num_tree)
        learner$train(task)
      }
      else{
        data=as.data.frame(da_rf2())
        train=cbind(data[input$tabe1_c5],data[input$tabe1_c6])
        max_depth=as.numeric(input$tabe1_t5)
        min_node_size=as.numeric(input$tabe1_t6)
        mtry=as.numeric(input$tabe1_t7)
        num_tree=as.numeric(input$tabe1_t8)
        task = as_task_regr(train,target = input$tabe1_c6)
        learner = lrn('regr.ranger',max.depth=max_depth,min.node.size=min_node_size,
                      mtry=mtry,importance = "impurity",num.trees=num_tree)
        learner$train(task)
      }
    }
  )
  observeEvent(
    input$tabe1_a2,
    {
      importance=rf()$importance()/sum(rf()$importance())
      res=c(list(rf()$model),list(importance))
      names(res)=c('model','feature.importance')
      output$tabe1_p3=renderPrint({
        res
      })
      output$tabe1_plot1=renderPlot({
        barplot(importance,xlab = 'feature',ylab = 'importance',cex.names = 0.8,cex.axis = 0.8)
      })
    }
  )
  da_rf3<-reactive({
    req(input$tabe1_f3)
    read_csv(input$tabe1_f3$datapath)
  })
  da_rf4<-reactive({
    req(da_rf3())
    if(input$tabe1_c7=='classification'){
      tryCatch(
        {
          data=as.data.frame(da_rf3())
          data[[input$tabe1_c6]]=factor(data[[input$tabe1_c6]])
          return(data)
        },error=function(e){
          return(da_rf3())
        }
      )
    }
    else{
      return(da_rf3())
    }
  })
  observeEvent(
    input$tabe1_a3,
    {
      if(input$tabe1_c7=='classification'){
        res=rf()$predict_newdata(da_rf4())
        output$tabe1_p4<-renderPrint({
          print(res)
        })
        if(input$tabe1_c8%in%c('classif.acc','classif.auc')){
          tryCatch(
            {
              output$tabe1_p5<-renderPrint({
                print(res$score(msr(input$tabe1_c8)))
              })
            },error=function(e){
              output$tabe1_p5<-renderPrint({
                print('An error occurred since there is no real y.')
              })
            }
          )
        }
        else if(input$tabe1_c8%in%c('prob','response','confusion')){
          output$tabe1_p5<-renderPrint({
            print(res[[input$tabe1_c8]])
          })
        }
        test = as.data.frame(da_rf3())
        test1 = test[c(input$tabe1_c5,input$tabe1_c6)]
        test1$pred = res$response
        output$tabe1_d1=downloadHandler(
          'rf.csv',
          content = function(file){
            write.csv(test1,file,row.names = F)
          }
        )
      }
      else{
        res=rf()$predict_newdata(da_rf4())
        output$tabe1_p4<-renderPrint({
          print(res)
        })
        if(input$tabe1_c8%in%c('regr.mse','regr.rmse','regr.mae')){
          tryCatch(
            {
              output$tabe1_p5<-renderPrint({
                print(res$score(msr(input$tabe1_c8)))
              })
            },error=function(e){
              output$tabe1_p5<-renderPrint({
                print('An error occurred since there is no real y.')
              })
            }
          )
        }
        else if(input$tabe1_c8%in%c('response','truth')){
          output$tabe1_p5<-renderPrint({
            print(res[[input$tabe1_c8]])
          })
        }
        test = as.data.frame(da_rf3())
        test1 = test[c(input$tabe1_c5,input$tabe1_c6)]
        test1$pred = res$response
        output$tabe1_d1=downloadHandler(
          'rf.csv',
          content = function(file){
            write.csv(test1,file,row.names = F)
          }
        )
      }
    }
  )
  observeEvent(
    input$tabe1_a3,
    {
      if(input$tabe1_c7=='classification'){
        tryCatch(
          {
            res=rf()$predict_newdata(da_rf4())
            da=data.frame(da_rf4()[input$tabe1_c6],res$response,res$prob[,2])
            output$tabe1_plot2<-renderPlot({
              ks.plot(da)
            })
            output$tabe1_plot3<-renderPlot({
              roc.plot(da)
            })
          },error=function(e){
            output$tabe1_p6<-renderPrint({
              print('An error occurred since there is no real y.')
            })
          }
        )
      }
    }
  )
  
  
  # Adaboost
  da_ada1<-reactive({
    req(input$tabe2_f1)
    read_csv(input$tabe2_f1$datapath)
  })
  da_ada2<-reactive({
    req(input$tabe2_f2)
    read_csv(input$tabe2_f2$datapath)
  })
  observe({
    req(da_ada1())
    updateSelectInput(session,'tabe2_c1',choices = names(da_ada1()),selected = names(da_ada1())[-length(names(da_ada1()))])
    updateSelectInput(session,'tabe2_c2',choices = names(da_ada1()),selected = names(da_ada1())[length(names(da_ada1()))])
  })
  ada1 = eventReactive(
    input$tabe2_a1,
    {
      train = as.data.frame(da_ada1())
      train[[input$tabe2_c2]] = factor(train[[input$tabe2_c2]])
      cv = as.numeric(input$tabe2_t1)
      maxdepth = as.numeric(input$tabe2_t2)
      minsplit = as.numeric(input$tabe2_t3)
      minbucket = as.numeric(input$tabe2_t4)
      if(input$tabe2_c3=='tune'){
        fit = boosting.cv(as.formula(paste(input$tabe2_c2,'~',paste(input$tabe2_c1,collapse = '+'))),
                           data = train,
                           v = cv,
                           control = rpart.control(minsplit = minsplit,maxdepth = maxdepth,minbucket = minbucket))
      }
      else{
        fit = boosting(as.formula(paste(input$tabe2_c2,'~',paste(input$tabe2_c1,collapse = '+'))),
                       data = train,
                       control = rpart.control(minsplit = minsplit,maxdepth = maxdepth,minbucket = minbucket))
      }
      return(fit)
    }
  )
  observeEvent(
    input$tabe2_a1,
    {
      if(input$tabe2_c3=='tune'){
        res = c(list(ada1()$error))
        names(res) = c('error rate')
      }
      else{
        fit = ada1()
        res = c(list(fit$importance),list(fit$weights),list(fit$class))
        names(res) = c('importance','weights','class')
      }
      output$tabe2_p1 = renderPrint({
        res
      })
    }
  )
  observeEvent(
    input$tabe2_a2,
    {
      test = as.data.frame(da_ada2())
      if(input$tabe2_c4=='auc' & input$tabe2_c3=='fit'){
        res = predict(ada1(),test)
        prob = res$prob[,2]
        y = test[[input$tabe2_c2]]
        output$tabe2_p2=renderPrint({
          diy_auc(y,prob)
        })
      }
      else if(input$tabe2_c4!='auc' & input$tabe2_c3=='fit'){
        res = predict(ada1(),test)
        output$tabe2_p2 = renderPrint({
          res[[input$tabe2_c4]]
        })
      }
    }
  )
  observeEvent(
    input$tabe2_a3,
    {
      if(input$tabe2_c3=='fit'){
        test = as.data.frame(da_ada2())
        res = predict(ada1(),test)
        da=data.frame(test[input$tabe2_c2],as.numeric(res$class),res$prob[,2])
        output$tabe2_plot2<-renderPlot({
          ks.plot(da)
        })
        output$tabe2_plot1<-renderPlot({
          roc.plot(da)
        })
        test1 = test[c(input$tabe2_c1,input$tabe2_c2)]
        test1$pred = res$class
        output$tabe2_d1=downloadHandler(
          'adaboost.csv',
          content = function(file){
            write.csv(test1,file,row.names = F)
          }
        )
      }
    }
  )
  
  # XGBoost
  
  da_xgb1<-reactive({
    req(input$tabe3_f1)
    read_csv(input$tabe3_f1$datapath)
  })
  observe({
    req(da_xgb1())
    updateSelectInput(session,'tabe3_c1',choices = names(da_xgb1()),selected = names(da_xgb1())[-length(names(da_xgb1()))])
    updateSelectInput(session,'tabe3_c2',choices = names(da_xgb1()),selected = names(da_xgb1())[length(names(da_xgb1()))])
  })
  tune_xgb = eventReactive(
    input$tabe3_a1,
    {
      data=as.data.frame(da_xgb1())
      train=cbind(data[input$tabe3_c1],data[input$tabe3_c2])
      max_depth = as.numeric(unlist(strsplit(input$tabe3_t1,',')))
      eta = as.numeric(unlist(strsplit(input$tabe3_t2,',')))
      gamma = as.numeric(unlist(strsplit(input$tabe3_t3,',')))
      lambda = as.numeric(unlist(strsplit(input$tabe3_t4,',')))
      if(input$tabe3_c3=='classification'){
        task = as_task_classif(train,target = input$tabe3_c2)
        learner = lrn('classif.xgboost',max_depth = to_tune(max_depth[1],max_depth[2]),
                      eta = to_tune(seq(eta[1],eta[2],by = eta[3])),
                      gamma = to_tune(seq(gamma[1],gamma[2],by = gamma[3])),
                      lambda = to_tune(seq(lambda[1],lambda[2],by = lambda[3])),
                      predict_type='prob')
        instance = tune(
          tuner = tnr("grid_search"),
          task = task,
          learner = learner,
          resampling = rsmp("cv", folds = 5),
          measure = msr("classif.auc"))
      }
      else{
        task = as_task_regr(train,target = input$tabe3_c2)
        learner = lrn('regr.xgboost',max_depth = to_tune(max_depth[1],max_depth[2]),
                      eta = to_tune(seq(eta[1],eta[2],by = eta[3])),
                      gamma = to_tune(seq(gamma[1],gamma[2],by = gamma[3])),
                      lambda = to_tune(seq(lambda[1],lambda[2],by = lambda[3])))
        instance = tune(
          tuner = tnr("grid_search"),
          task = task,
          learner = learner,
          resampling = rsmp("cv", folds = 5),
          measure = msr("regr.mse"))
      }
    }
  )
  observeEvent(
    input$tabe3_a2,
    {
      req(tune_xgb())
      output$tabe3_p1<-renderPrint({
        print(tune_xgb())
      })
      output$tabe3_p2<-renderPrint({
        tune_xgb()[[input$tabe3_c4]]
      })
    }
  )
  da_xgb2<-reactive({
    req(input$tabe3_f2)
    read_csv(input$tabe3_f2$datapath)
  })
  observe({
    req(da_xgb2())
    updateSelectInput(session,'tabe3_c5',choices = names(da_xgb2()),selected = names(da_xgb2())[-length(names(da_xgb2()))])
    updateSelectInput(session,'tabe3_c6',choices = names(da_xgb2()),selected = names(da_xgb2())[length(names(da_xgb2()))])
  })
  xgb1=eventReactive(
    input$tabe3_a3,
    {
      data=as.data.frame(da_xgb2())
      train=cbind(data[input$tabe3_c5],data[input$tabe3_c6])
      max_depth=as.numeric(input$tabe3_t5)
      eta=as.numeric(input$tabe3_t6)
      gamma=as.numeric(input$tabe3_t7)
      lambda=as.numeric(input$tabe3_t8)
      if(input$tabe3_c7=='classification'){
        task = as_task_classif(train,target = input$tabe3_c6)
        learner = lrn('classif.xgboost',max_depth=max_depth,eta=eta,
                      gamma=gamma,lambda = lambda,predict_type='prob')
        learner$train(task)
      }
      else{
        task = as_task_regr(train,target = input$tabe3_c6)
        learner = lrn('regr.xgboost',max_depth=max_depth,eta=eta,
                      gamma=gamma,lambda = lambda)
        learner$train(task)
      }
    }
  )
  observeEvent(
    input$tabe3_a3,
    {
      importance=xgb1()$importance()
      res=c(list(xgb1()$model),list(importance))
      names(res)=c('model','feature.importance')
      output$tabe3_p3=renderPrint({
        res
      })
      output$tabe3_plot1=renderPlot({
        barplot(importance,xlab = 'feature',ylab = 'importance',cex.names = 0.8,cex.axis = 0.8)
      })
    }
  )
  da_xgb3<-reactive({
    req(input$tabe3_f3)
    read_csv(input$tabe3_f3$datapath)
  })
  da_xgb4<-reactive({
    req(da_xgb3())
    if(input$tabe3_c7=='classification'){
      tryCatch(
        {
          data=as.data.frame(da_xgb3())
          data[[input$tabe3_c6]]=factor(data[[input$tabe3_c6]])
          return(data)
        },error=function(e){
          return(da_xgb3())
        }
      )
    }
    else{
      return(da_xgb3())
    }
  })
  observeEvent(
    input$tabe3_a4,
    {
      if(input$tabe3_c7=='classification'){
        res=xgb1()$predict_newdata(da_xgb4())
        output$tabe3_p4<-renderPrint({
          print(res)
        })
        test = as.data.frame(da_xgb3())
        test1 = test[c(input$tabe3_c5,input$tabe3_c6)]
        test1$pred = res$response
        output$tabe3_d1=downloadHandler(
          'xgboost.csv',
          content = function(file){
            write.csv(test1,file,row.names = F)
          }
        )
        if(input$tabe3_c8%in%c('classif.acc','classif.auc')){
          tryCatch(
            {
              output$tabe3_p5<-renderPrint({
                print(res$score(msr(input$tabe3_c8)))
              })
            },error=function(e){
              output$tabe3_p5<-renderPrint({
                print('An error occurred since there is no real y.')
              })
            }
          )
        }
        else if(input$tabe3_c8%in%c('prob','response','confusion')){
          output$tabe3_p5<-renderPrint({
            print(res[[input$tabe3_c8]])
          })
        }
      }
      else{
        res=xgb1()$predict_newdata(da_xgb4())
        output$tabe3_p4<-renderPrint({
          print(res)
        })
        test = as.data.frame(da_xgb3())
        test1 = test[c(input$tabe3_c5,input$tabe3_c6)]
        test1$pred = res$response
        output$tabe3_d1=downloadHandler(
          'xgboost.csv',
          content = function(file){
            write.csv(test1,file,row.names = F)
          }
        )
        if(input$tabe3_c8%in%c('regr.mse','regr.rmse','regr.mae')){
          tryCatch(
            {
              output$tabe3_p5<-renderPrint({
                print(res$score(msr(input$tabe3_c8)))
              })
            },error=function(e){
              output$tabe3_p5<-renderPrint({
                print('An error occurred since there is no real y.')
              })
            }
          )
        }
        else if(input$tabe3_c8%in%c('response','truth')){
          output$tabe3_p5<-renderPrint({
            print(res[[input$tabe3_c8]])
          })
        }
      }
    }
  )
  observeEvent(
    input$tabe3_a4,
    {
      if(input$tabe3_c7=='classification'){
        tryCatch(
          {
            res=xgb1()$predict_newdata(da_xgb4())
            da=data.frame(da_xgb4()[input$tabe3_c6],res$response,res$prob[,2])
            output$tabe3_plot2<-renderPlot({
              ks.plot(da)
            })
            output$tabe3_plot3<-renderPlot({
              roc.plot(da)
            })
          },error=function(e){
            output$tabe3_p6<-renderPrint({
              print('An error occurred since there is no real y.')
            })
          }
        )
      }
    }
  )
  
  # LightGBM
  
  da_lgb1<-reactive({
    req(input$tabe4_f1)
    read_csv(input$tabe4_f1$datapath)
  })
  da_lgb2<-reactive({
    req(input$tabe4_f2)
    read_csv(input$tabe4_f2$datapath)
  })
  observe({
    req(da_lgb1())
    updateSelectInput(session,'tabe4_c1',choices = names(da_lgb1()),selected = names(da_lgb1())[-length(names(da_lgb1()))])
    updateSelectInput(session,'tabe4_c2',choices = names(da_lgb1()),selected = names(da_lgb1())[length(names(da_lgb1()))])
  })
  lgb1 = eventReactive(
    input$tabe4_a1,
    {
      train = as.matrix(da_lgb1())
      lgbtrain = lgb.Dataset(data = train[,input$tabe4_c1],label = train[,input$tabe4_c2])
      max_depth = as.numeric(input$tabe4_t1)
      eta = as.numeric(input$tabe4_t2)
      ff = as.numeric(input$tabe4_t3)
      lambda = as.numeric(input$tabe4_t4)
      if(input$tabe4_c3=='tune'){
        if(input$tabe4_c4=='binary'){
          params = list(
            objective = 'binary',
            learning_rate = eta,
            lambda = lambda,
            max_depth = max_depth,
            metric = "auc",
            feature_fraction = ff
          )
          res = lgb.cv(data = lgbtrain,params = params,nfold = 10)
          return(res)
        }
        else if(input$tabe4_c4=='regression'){
          params = list(
            objective = 'regression',
            learning_rate = eta,
            lambda = lambda,
            max_depth = max_depth,
            metric = "rmse",
            feature_fraction = ff
          )
          res = lgb.cv(data = lgbtrain,params = params,nfold = 10)
          return(res)
        }
      }
      else{
        if(input$tabe4_c4=='binary'){
          params = list(
            objective = 'binary',
            learning_rate = eta,
            lambda = lambda,
            max_depth = max_depth,
            metric = "auc",
            feature_fraction = ff
          )
          res = lgb.train(data = lgbtrain,params = params)
          return(res)
        }
        else if(input$tabe4_c4=='regression'){
          params = list(
            objective = 'regression',
            learning_rate = eta,
            lambda = lambda,
            max_depth = max_depth,
            metric = "rmse",
            feature_fraction = ff
          )
          res = lgb.train(data = lgbtrain,params = params)
          return(res)
        }
      }
    }
  )
  observeEvent(
    input$tabe4_a2,
    {
      res = lgb1()
      test = as.data.frame(da_lgb2())
      test1 = as.matrix(test[input$tabe4_c1])
      if(input$tabe4_c3=='tune'){
        output$tabe4_p1=renderPrint({
          res$best_score
        })
      }
      else{
        if(input$tabe4_c4=='binary'){
          prob = predict(res,test1)
          ypred = ifelse(prob>=0.5,1,0)
          predi = data.frame(ypred,prob)
          score = diy_auc(test[[input$tabe4_c2]],prob)
          result = c(list(score),list(predi))
          names(result) = c('auc','prediction')
          output$tabe4_p1=renderPrint({
            result
          })
          test$pred = ypred
          output$tabe4_d1 = downloadHandler(
            'lgb.csv',
            content = function(file){
              write.csv(test,file,row.names = F)
            }
          )
          da=data.frame(test[[input$tabe4_c2]],ypred,prob)
          output$tabe4_plot1<-renderPlot({
            ks.plot(da)
          })
          output$tabe4_plot2<-renderPlot({
            roc.plot(da)
          })
        }
        else if(input$tabe4_c4=='regression'){
          ypred = predict(res,test1)
          rmse = rerror(test[[input$tabe4_c2]],ypred,type = 'rmse')
          result = c(list(rmse),list(ypred))
          names(result) = c('rmse','prediction')
          output$tabe4_p1=renderPrint({
            result
          })
          test$pred = ypred
          output$tabe4_d1 = downloadHandler(
            'lgb.csv',
            content = function(file){
              write.csv(test,file,row.names = F)
            }
          )
        }
      }
    }
  )
  
  # VaR & ES
  
  da_var<-reactive({
    req(input$tabf4_f1)
    read_csv(input$tabf4_f1$datapath)
  })
  observe({
    req(da_var())
    updateSelectInput(session,'tabf4_c1',choices = names(da_var()))
  })
  observeEvent(
    input$tabf4_a1,
    {
      data = as.data.frame(da_var())
      return = data[[input$tabf4_c1]]
      alpha = as.numeric(input$tabf4_t1)
      if(input$tabf4_c2=='Nonparametric'){
        VaR = quantile(return,alpha)
        ES = mean(return[return<VaR])
      }
      else{
        if(input$tabf4_c3=='Gaussian'){
          mu = mean(return)
          sd = sd(return)
          VaR = qnorm(alpha, mean = mu, sd = sd)
          ES = -mu + sd * dnorm(qnorm(alpha)) / alpha
        }
        else{
          fit = fitdistr(return,densfun = 't')
          mu = fit$estimate[1]
          lambda = fit$estimate[2]
          df = fit$estimate[3]
          q = qt(alpha,df = df)
          VaR = q*lambda+mu
          ES = -mu+lambda*dt(q, df = df)*(df+q^2)/(df-1)/alpha
        }
      }
      result = c(list(VaR),list(ES))
      names(result) = c('Value.at.Risk','Expected.Shortfall')
      output$tabf4_p1<-renderPrint({
        result
      })
    }
  )
  
  # Modern Portfolio Theory
  observeEvent(
    input$tabf3_a1,
    {
      mu1=as.numeric(input$tabf3_t1)
      mu2=as.numeric(input$tabf3_t2)
      sigma1=as.numeric(input$tabf3_t3)
      sigma2=as.numeric(input$tabf3_t4)
      p=as.numeric(input$tabf3_t5)
      output$tabf3_p1<-renderPrint({
        Portfolio2(mu1,mu2,sigma1,sigma2,p)
      })
      res=Portfolio2(mu1,mu2,sigma1,sigma2,p)
      mv=res[[3]]
      sigma=seq(mv,mv+2,by=0.001)
      a=sigma1^2-2*p*sigma1*sigma2+sigma2^2
      b=mu2*sigma1^2-(mu1+mu2)*p*sigma1*sigma2+mu1*sigma2^2
      c=mu2^2*sigma1^2-2*p*mu1*mu2*sigma1*sigma2+mu1^2*sigma2^2
      mu_top=b/a+sqrt((mu1-mu2)^2/a*sigma^2-c/a+b^2/a^2)
      mu_bottom=b/a-sqrt((mu1-mu2)^2/a*sigma^2-c/a+b^2/a^2)
      data=data.frame(sigma,mu_top,mu_bottom)
      output$tabf3_plot1<-renderPlot({
        ggplot(data)+
          geom_line(aes(x=sigma,y=mu_top,linetype='efficient frontier'))+
          geom_line(aes(x=sigma,y=mu_bottom,linetype='2'))+
          geom_point(aes(x=res[[3]],y=res[[2]]),shape=17,size=2,color='red')+
          geom_text(aes(x=res[[3]]+0.14,y=res[[2]]),label='MVP',size=4,color='red')+
          labs(y='mu')+
          scale_linetype_manual(values = c('efficient frontier'=1,'2'=2))+
          theme(
            legend.title = element_blank()
          )
      })
    }
  )
  
  da_mpt1<-reactive({
    req(input$tabf3_f1)
    read_csv(input$tabf3_f1$datapath)
  })
  observe({
    req(da_mpt1())
    updateSelectInput(session,'tabf3_c1',choices = names(da_mpt1()))
  })
  observeEvent(
    input$tabf3_a2,
    {
      tryCatch(
        {
          data=as.data.frame(da_mpt1()[input$tabf3_c1])
          mu=as.matrix(apply(data,2,mean))
          sigma=cov(data)
          targetmu=as.numeric(input$tabf3_t6)
          output$tabf3_p2<-renderPrint({
            print(Portfolion(targetmu,mu,sigma))
          })
          res=Portfolion(targetmu,mu,sigma)
          one=matrix(1,nrow = length(mu),ncol = 1)
          sigmaInv=solve(sigma)
          A=as.numeric(t(mu)%*%sigmaInv%*%one)
          B=as.numeric(t(mu)%*%sigmaInv%*%mu)
          C=as.numeric(t(one)%*%sigmaInv%*%one)
          D=B*C-A^2
          sigma1=seq(sqrt(res[[6]]),sqrt(res[[6]])+2.5*sqrt(res[[6]]),length.out=1000)
          mu_up=A/C+sqrt((D*sigma1^2+A^2/C-B)/C)
          mu_bottom=A/C-sqrt((D*sigma1^2+A^2/C-B)/C)
          output$tabf3_plot2=renderPlot({
            ggplot()+
              geom_line(aes(x=sigma1,y=mu_up,linetype='efficient frontier'))+
              geom_line(aes(x=sigma1,y=mu_bottom,linetype='2'))+
              geom_hline(yintercept = res[[1]],linetype=3)+
              geom_ribbon(aes(x=sigma1,ymin=mu_bottom,ymax=mu_up),fill='skyblue',alpha=0.3)+
              geom_point(aes(x=sqrt(res[[6]]),y=res[[4]]),shape=17,size=2,color='red')+
              geom_point(aes(x=sqrt(res[[3]]),y=res[[1]]),shape=18,size=2,color='orange')+
              geom_text(aes(x=sqrt(res[[6]])+0.15*sqrt(res[[6]]),y=res[[4]]),label='MVP',size=4,color='red')+
              geom_text(aes(x=sqrt(res[[6]])+1.25*sqrt(res[[6]]),y=res[[4]]),label='feasible region',size=4,color='black')+
              geom_text(aes(x=1.15*sqrt(res[[3]]),y=res[[1]]),label='MEP',size=4,color='orange')+
              labs(y='mu',x='sigma')+
              scale_linetype_manual(values = c('efficient frontier'=1,'2'=2))+
              theme(
                legend.title = element_blank()
              )
          })
          n=length(mu)
          F1=cbind(mu,rep(1,n),diag(1,n))
          a=c(targetmu,1,rep(0,n))
          w0=matrix(a,nrow=length(a))
          output$tabf3_p4<-renderPrint({
            solve.QP(sigma,rep(0,n),F1,w0,2)
          })
        },error=function(e){
          output$tabf3_p3=renderPrint({
            'An error occurred, please check your data.'
          })
        }
      )
    }
    
  )
  observeEvent(
    input$tabf3_rfa1,
    {
      tryCatch(
        {
          mu0=as.numeric(input$tabf3_rf1)
          mu1=as.numeric(input$tabf3_rf2)
          mu2=as.numeric(input$tabf3_rf3)
          sigma1=as.numeric(input$tabf3_rf4)
          sigma2=as.numeric(input$tabf3_rf5)
          p=as.numeric(input$tabf3_rf6)
          targetmu=as.numeric(input$tabf3_rf7)
          output$tabf3_rfp1<-renderPrint({
            PortfolioRF(mu0,mu1,mu2,sigma1,sigma2,p,targetmu)
          })
          output$tabf3_rfplot1<-renderPlot({
            PortfolioRFPlot(mu0,mu1,mu2,sigma1,sigma2,p,targetmu)
          })
        },error=function(e){
          output$tabf3_rfp1<-renderPrint({
            'An error occurred, please check your input!'
          })
        }
      )
    }
  )
  da_mep<-reactive({
    req(input$tabf3_file1)
    read_csv(input$tabf3_file1$datapath)
  })
  observe({
    req(da_mep())
    updateSelectInput(session,'tabf3_select1',choices = names(da_mep()))
  })
  observeEvent(
    input$tabf3_action1,
    {
      tryCatch(
        {
          data=as.data.frame(da_mep()[,input$tabf3_select1])
          sigma=cov(data)
          mu=as.matrix(apply(data,2,mean))
          mu0=as.numeric(input$tabf3_text1)
          targetmu=as.numeric(input$tabf3_text2)
          output$tabf3_print1<-renderPrint({
            EffPortfilio(mu0,targetmu,mu,sigma)
          })
        },error=function(e){
          output$tabf3_print1<-renderPrint({'An error occurred, please check your data.'})
        }
      )
      
    }
  )
  da_cp<-reactive({
    req(input$tabf3_cpf1)
    read_csv(input$tabf3_cpf1$datapath)
  })
  observe({
    req(da_cp())
    updateSelectInput(session,'tabf3_cpc1',choices = names(da_cp()))
  })
  observeEvent(
    input$tabf3_cpa1,
    {
      dat = as.matrix(da_cp()[input$tabf3_cpc1])
      m = length(input$tabf3_cpc1)
      n = dim(dat)[1]
      y = matrix(nrow = n,ncol = m)
      for(j in 1:m){
        Fj = ecdf(dat[,j])
        y[,j] = qnorm(Fj(dat[,j]))
      }
      rem = c()
      for(j in 1:m){
        rem[j] = which(y[,j]==Inf)
      }
      y1 = y[-rem,]
      output$tabf3_cpp1<-renderPrint({
        cor(y1)
      })
    }
  )
  observeEvent(
    input$tabf3_cpa2,
    {
      dat = as.matrix(da_cp()[input$tabf3_cpc1])
      m = length(input$tabf3_cpc1)
      sr = as.numeric(unlist(strsplit(input$tabf3_cpt1,',')))
      df = seq(sr[1], sr[2], by = sr[3])
      n1 = length(df)
      loglik = rep(0, n1)
      for(i in 1:n1){
        fit = cov.trob(dat, nu = df[i])
        loglik[i] = sum(log(dmt(dat,mean = fit$center,S = fit$cov, df = df[i])))
      }
      best.df = df[which(loglik==max(loglik))]
      output$tabf3_cpp2=renderPrint({
        cat('The best df is:', best.df)
      })
      n = dim(dat)[1]
      y = matrix(nrow = n,ncol = m)
      for(j in 1:m){
        Fj = ecdf(dat[,j])
        y[,j] = qnorm(Fj(dat[,j]))
      }
      rem = c()
      for(j in 1:m){
        rem[j] = which(y[,j]==Inf)
      }
      y1 = y[-rem,]
      S = cor(y1)
      fit = cov.trob(dat, nu = best.df)
      # set.seed(2015)
      y2 = rmnorm(n=1000, mean=rep(0,m), varcov=S)
      for(j in 1:m){
        y2[,j] = pnorm(y2[,j])
      }
      for(j in 1:m){
        y2[,j] = fit$center[j]+qt(y2[,j],df = best.df)*sqrt(fit$cov[j,j])
      }
      w = as.matrix(as.numeric(unlist(strsplit(input$tabf3_cpt3,','))))
      return = y2 %*% w
      alpha = as.numeric(input$tabf3_cpt2)
      VaR = quantile(return, alpha)
      ES = mean(return[return<VaR])
      output$tabf3_cpp3<-renderPrint({
        cat('VaR =',VaR,'\n','ES =',ES)
      })
    } 
    
  )
  
  # CAPM
  da_capm1<-reactive({
    req(input$tabf1_f1)
    read_csv(input$tabf1_f1$datapath)
  })
  observe({
    req(da_capm1())
    updateSelectInput(session,'tabf1_c1',choices = names(da_capm1()),selected = names(da_capm1())[4:length(names(da_capm1()))])
    updateSelectInput(session,'tabf1_c2',choices = names(da_capm1()),selected = names(da_capm1())[2])
    updateSelectInput(session,'tabf1_c3',choices = names(da_capm1()),selected = names(da_capm1())[3])
  })
  observeEvent(
    input$tabf1_a1,
    {
      req(da_capm1())
      output$tabf1_table1<-renderDT(
        datatable(
          da_capm1(),
          options = list(
            columnDefs=list(list(className='dt-center',targets=1:length(colnames(da_capm1())))),
            scrollX=T,
            lengthMenu=c(10,20,50)
          )
        )
      )
    }
  )
  observeEvent(
    input$tabf1_a1,
    {
      tryCatch(
        {
          data=as.data.frame(da_capm1())
          stock=input$tabf1_c1
          for(i in 1:length(stock)){
            data[[stock[i]]]=data[[stock[i]]]-data[[input$tabf1_c2]]
          }
          data[[input$tabf1_c3]]=data[[input$tabf1_c3]]-data[[input$tabf1_c2]]
          li=list()
          for(i in 1:length(stock)){
            fit=lm(as.formula(paste(stock[i],'~',input$tabf1_c3,'-1')),data=data)
            li=c(li,list(summary(fit)$coefficients))
          }
          names(li)=stock
          da1=matrix(nrow = 3,ncol = 2)
          li1=list()
          for(i in 1:length(stock)){
            fit=lm(as.formula(paste(stock[i],'~',input$tabf1_c3)),data=data)
            li1=c(li1,list(summary(fit)$coefficients))
            da1[i,]=c(summary(fit)$coefficients[1,1],summary(fit)$coefficients[1,4])
          }
          names(li1)=stock
          colnames(da1)=c('Estimate','p-value')
          rownames(da1)=stock
          df=data.frame(da1)
          df1=df[df[,2]<0.05,]
          df1[df1[,1]==max(df1[,1]),]
          max_alpha=list(max_alpha=df1[df1[,1]==max(df1[,1]),])
          output$tabf1_p2=renderPrint({
            print(li)
          })
          output$tabf1_p3=renderPrint({
            print(max_alpha)
          })
          output$tabf1_p4=renderPrint(
            width = 1200,
            {
              print(li1)
            })
        },error=function(e){
          output$tabf1_p2=renderPrint({
            print('An error occurred, please check your data structure.')
          })
          output$tabf1_p3=renderPrint({
            print('An error occurred, please check your data structure.')
          })
          output$tabf1_p4=renderPrint({
            print('An error occurred, please check your data structure.')
          })
        }
      )
    }
  )
  observeEvent(
    input$tabf1_a1,
    {
      tryCatch(
        {
          data=as.data.frame(da_capm1())
          stock=input$tabf1_c1
          li=list()
          for(i in 1:length(stock)){
            li=c(li,cov(data[[stock[i]]],data[[input$tabf1_c3]])/var(data[[input$tabf1_c3]]))
          }
          names(li)=stock
          output$tabf1_p1=renderPrint({
            print(li)
          })
        },error=function(e){
          output$tabf1_p1=renderPrint({
            print('An error occurred, please check your data structure.')
          })
        }
      )
    }
  )
  
  # Time Series
  
  da_ts<-reactive({
    req(input$tabf2_f2)
    read_csv(input$tabf2_f2$datapath)
  })
  observe({
    req(da_ts())
    n=length(names(da_ts()))
    updateSelectInput(session,'tabf2_c3',choices = names(da_ts()),selected = names(da_ts())[n])
  })
  observeEvent(
    input$tabf2_a2,
    {
      data=as.data.frame(da_ts())
      if(input$tabf2_c4=='No'){
        output$tabf2_plot1<-renderPlot({
          plot(data[[input$tabf2_c3]],type = 'l')
        })
        output$tabf2_plot2<-renderPlot({
          acf(data[input$tabf2_c3],main = '')
        })
        res=list()
        for(i in 1:6){
          res=c(res,list(adfTest(data[[input$tabf2_c3]],lags = i,type='c')))
        }
        output$tabf2_p3<-renderPrint({
          print(res)
        })
      }
      else{
        k=as.numeric(input$tabf2_t3)
        ori=data[[input$tabf2_c3]]
        series=diff(ori,differences = k)
        output$tabf2_plot1<-renderPlot({
          plot(series,type = 'l')
        })
        output$tabf2_plot2<-renderPlot({
          acf(series,main = '')
        })
        res=list()
        for(i in 1:6){
          res=c(res,list(adfTest(series,lags = i,type='c')))
        }
        output$tabf2_p3<-renderPrint({
          print(res)
        })
      }
    }
  )
  
  
  da_arima<-reactive({
    req(input$tabf2_f1)
    read_csv(input$tabf2_f1$datapath)
  })
  da_arima1<-reactive({
    req(input$tabf2_f3)
    read_csv(input$tabf2_f3$datapath)
  })
  observe({
    req(da_arima())
    n=length(names(da_arima()))
    updateSelectInput(session,'tabf2_c1',choices = names(da_arima()),selected = names(da_arima())[n])
  })
  
  observeEvent(
    input$tabf2_a3,
    {
      select.arma<-function(p,q,series){
        res = matrix(0,nrow = p*q,ncol = 3)
        r = 1
        for(i in 1:p){
          for(j in 1:q){
            model = arima(series,c(i,0,j))
            aic = model$aic
            res[r,1] = i
            res[r,2] = j
            res[r,3] = aic
            r = r + 1
          }
        }
        colnames(res) = c('p','q','aic')
        best_model = res[res[,3]==min(res[,3]),]
        result = c(list(res),list(best_model))
        names(result) = c('fit.result','best.model')
        return(result)
      }
      p=as.numeric(input$tabf2_t4)
      q=as.numeric(input$tabf2_t5)
      series=da_arima()[[input$tabf2_c1]]
      output$tabf2_p4<-renderPrint({
        select.arma(p,q,series)
      })
    }
  )
  arima1=eventReactive(
    input$tabf2_a1,
    {
      req(input$tabf2_t1,input$tabf2_c2)
      str1=input$tabf2_t1
      series=da_arima()[[input$tabf2_c1]]
      order=as.numeric(unlist(strsplit(str1,',')))
      if(input$tabf2_c2=='yes'){
        arima(series,order = order,include.mean = T)
      }
      else{
        arima(series,order = order,include.mean = F)
      }
    }
  )
  observeEvent(
    input$tabf2_a1,
    {
      output$tabf2_p1<-renderPrint({
        arima1()
      })
      output$tabf2_p2<-renderPrint({
        predict(arima1(),n.ahead = as.numeric(input$tabf2_t2))
      })
    }
  )
  observeEvent(
    input$tabf2_a4,
    {
      req(input$tabf2_c5,arima1(),input$tabf2_t2)
      plot1=input$tabf2_c5
      if(plot1=='residuals plot'){
        output$tabf2_plot3=renderPlot({
          plot(arima1()$residuals,type="l",ylab="residuals",xlab="time",main=plot1)
        })
      }
      else if(plot1=='residuals acf plot'){
        output$tabf2_plot3=renderPlot({
          acf(arima1()$residuals,main=plot1)
        })
      }
      else if(plot1=='squared residuals acf plot'){
        output$tabf2_plot3=renderPlot({
          acf(arima1()$residuals^2,main=plot1)
        })
      }
      else if(plot1=='squared residuals pacf plot'){
        output$tabf2_plot3=renderPlot({
          pacf(arima1()$residuals^2,main=plot1)
        })
      }
      else if(plot1=='residuals pacf plot'){
        output$tabf2_plot3=renderPlot({
          pacf(arima1()$residuals,main=plot1)
        })
      }
      resi=arima1()$residuals
      str1=input$tabf2_t1
      order=as.numeric(unlist(strsplit(str1,',')))
      LBtest=function(p,q,resi){
        result=list()
        lag=c()
        for(i in 1:3){
          res1=list(Box.test(resi,lag = i+p+q,fitdf = p+q,type = 'Ljung-Box'))
          result=c(result,res1)
          lag[i]=as.character(i+p+q)
        }
        lag=paste('lag =',lag)
        names(result)=lag
        return(result)
      }
      output$tabf2_p5<-renderPrint({
        LBtest(order[1],order[3],resi)
      })
      res=predict(arima1(),n.ahead = as.numeric(input$tabf2_t2))
      pred=as.vector(res$pred)
      y=as.vector(da_arima1()[[input$tabf2_c1]])
      result=rerror(y,pred,type = input$tabf2_c6)
      output$tabf2_p6<-renderPrint({
        cat('The',input$tabf2_c6,'is:',result)
      })
      
    }
  )
  
  da_garch<-reactive({
    req(input$tabf2_gf1)
    read_csv(input$tabf2_gf1$datapath)
  })
  da_garch1<-reactive({
    req(input$tabf2_gf2)
    read_csv(input$tabf2_gf2$datapath)
  })
  observe({
    req(da_garch())
    n=length(names(da_garch()))
    updateSelectInput(session,'tabf2_gc1',choices = names(da_garch()),selected = names(da_garch())[n])
  })
  garch1=eventReactive(
    input$tabf2_ga1,
    {
      ym=as.numeric(unlist(strsplit(input$tabf2_gt1,',')))
      arma.order=as.numeric(unlist(strsplit(input$tabf2_gt2,',')))
      ar=arma.order[1]
      ma=arma.order[2]
      data=da_garch()
      price<-ts(data[input$tabf2_gc1],frequency = 250,start = c(ym[1],ym[2]))
      spec = ugarchspec(variance.model = list(garchOrder=c(1,1)),mean.model = list(armaOrder=c(ar,ma),include.mean=T))
      fit=ugarchfit(spec,data=price)
      return(fit)
    }
  )
  observeEvent(
    input$tabf2_ga1,
    {
      output$tabf2_gp1=renderPrint({
        garch1()
      })
      pred=ugarchforecast(garch1(),n.ahead = as.numeric(input$tabf2_gt3))
      output$tabf2_gp2=renderPrint({
        pred
      })
      output$tabf2_gplot1=renderPlot({
        plot(garch1(),which=1)
      })
      output$tabf2_gplot2=renderPlot({
        plot(garch1(),which=3)
      })
      output$tabf2_gplot3=renderPlot({
        plot(pred,which=1)
      })
      output$tabf2_gplot4=renderPlot({
        plot(pred,which=3)
      })
      tryCatch(
        {
          y=da_garch1()[[input$tabf2_gc1]]
          ypred=as.vector(rugarch::sigma(pred))
          output$tabf2_gp3=renderPrint({
            cat('The rmse is:',rerror(y,ypred,type = 'rmse'))
          })
        },error=function(e){
          output$tabf2_gp3<-renderPrint({
            'No real y, rmse cannot be computed.'
          })
        }
      )
    }
  )
  da_mar<-reactive({
    req(input$tabf2_mf1)
    read_csv(input$tabf2_mf1$datapath)
  })
  da_mar1<-reactive({
    req(input$tabf2_mf2)
    read_csv(input$tabf2_mf2$datapath)
  })
  observe({
    req(da_mar())
    n=length(names(da_mar()))
    updateSelectInput(session,'tabf2_mc1',choices = names(da_mar()),selected = names(da_mar())[n])
  })
  mar=eventReactive(
    input$tabf2_ma1,
    {
      train = da_mar()[[input$tabf2_mc1]]
      order = as.numeric(unlist(strsplit(input$tabf2_mt1,',')))
      fit = fit_mixAR(train,model = order,fix = "shift")
      fit
    }
  )
  observeEvent(
    input$tabf2_ma1,
    {
      output$tabf2_mp1=renderPrint({
        mar()
      })
      train = da_mar()[[input$tabf2_mc1]]
      p=tsdiag(mar()$model,y=train,std.resid=T,ask=F,plot=F)
      output$tabf2_mp2=renderPrint({
        if(input$tabf2_mc2=='BIC'){
          p[[input$tabf2_mc2]]
        }
        else{
          p[[input$tabf2_mc2]][-1]
        }
      })
    }
  )
  observeEvent(
    input$tabf2_ma2,
    {
      req(da_mar(),da_mar1(),input$tabf2_mt1,input$tabf2_mc1,input$tabf2_mt2)
      train = da_mar()[[input$tabf2_mc1]]
      test = da_mar1()[[input$tabf2_mc1]]
      order = as.numeric(unlist(strsplit(input$tabf2_mt1,',')))
      m = max(order)
      p = as.numeric(input$tabf2_mt2)
      model = mar()$model
      res = pred.mar(train,test,m,model,p)
      output$tabf2_mp3<-renderPrint({
        res
      })
      index = which(test<res$VaR)
      x=seq(1,length(test))
      output$tabf2_mplot1=renderPlot({
        plot(test,type='l',lty=1,lwd=1.2,xlab='time',ylab="",col='black')
        lines(res$VaR,col='red',lwd=1.2)
        lines(res$ES,col='blue',lwd=1.2)
        points(x=x,y=test,col='black',pch=18,cex=0.7)
        points(x=x,y=res$VaR,col='red',pch=18,cex=0.7)
        points(x=x,y=res$ES,col='blue',pch=18,cex=0.7)
        points(x=index,y=test[index],col='orange',pch=18,cex=0.7)
        legend("topleft",c("real return","VaR","ES",expression("x"["t"]<VaR)),col=c("black","red","blue","orange"),
               pch=c(NA,NA,NA,18),lty=c(1,1,1,NA),cex=0.5)
      })
      output$tabf2_mp4<-renderPrint({
        VaRTest(p,test,res$VaR)
      })
      output$tabf2_mp5<-renderPrint({
        ESTest(alpha = 0.05,actual = test,ES = res$ES,VaR = res$VaR)
      })
    }
  )
  
  #GSMAR
  
  da_gsmar<-reactive({
    req(input$tabf2_gsf1)
    read_csv(input$tabf2_gsf1$datapath)
  })
  da_gsmar1<-reactive({
    req(input$tabf2_gsf2)
    read_csv(input$tabf2_gsf2$datapath)
  })
  observe({
    req(da_gsmar())
    n=length(names(da_gsmar()))
    updateSelectInput(session,'tabf2_gsc1',choices = names(da_gsmar()),selected = names(da_gsmar())[n])
  })
  gsmar=eventReactive(
    input$tabf2_gsa1,
    {
      train = da_gsmar()[[input$tabf2_gsc1]]
      com = as.numeric(input$tabf2_gst1)
      order = as.numeric(input$tabf2_gst2)
      a1=fitGSMAR(train,p = order,M = com,model = "StMAR",ncores = 6)
      s1=stmar_to_gstmar(a1,maxdf = 100)
      s1
    }
  )
  observeEvent(
    input$tabf2_gsa1,
    {
      output$tabf2_gsplot1=renderPlot({
        diagnostic_plot(gsmar())
      })
      output$tabf2_gsp1=renderPrint({
        summary(gsmar())
      })
      output$tabf2_gsp2=renderPrint({
        quantile_residual_tests(gsmar())
      })
    }
  )
  observeEvent(
    input$tabf2_gsa2,
    {
      train = da_gsmar()[[input$tabf2_gsc1]]
      test = da_gsmar1()[[input$tabf2_gsc1]]
      order = as.numeric(input$tabf2_gst2)
      p = as.numeric(input$tabf2_gst3)
      res = pred.gsmar(train,test,order,gsmar(),p)
      output$tabf2_gsp3<-renderPrint({
        res
      })
      index = which(test<res$VaR)
      x=seq(1,length(test))
      output$tabf2_gsplot2=renderPlot({
        plot(test,type='l',lty=1,lwd=1.2,xlab='time',ylab="",col='black')
        lines(res$VaR,col='red',lwd=1.2)
        lines(res$ES,col='blue',lwd=1.2)
        points(x=x,y=test,col='black',pch=18,cex=0.7)
        points(x=x,y=res$VaR,col='red',pch=18,cex=0.7)
        points(x=x,y=res$ES,col='blue',pch=18,cex=0.7)
        points(x=index,y=test[index],col='orange',pch=18,cex=0.7)
        legend("topleft",c("real return","VaR","ES",expression("x"["t"]<VaR)),col=c("black","red","blue","orange"),
               pch=c(NA,NA,NA,18),lty=c(1,1,1,NA),cex=0.5)
      })
      output$tabf2_gsp4<-renderPrint({
        VaRTest(p,test,res$VaR)
      })
      output$tabf2_gsp5<-renderPrint({
        ESTest(alpha = p,actual = test,ES = res$ES,VaR = res$VaR)
      })
    }
  )
}