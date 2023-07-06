# diy auc
diy_auc<-function(y,prob){
  data=data.frame(y,prob)
  posi_prob=data[data$y==1,2]
  nega_prob=data[data$y==0,2]
  total=length(posi_prob)*length(nega_prob)
  cnt=0
  for(i in 1:length(posi_prob)){
    for(j in 1:length(nega_prob)){
      if(posi_prob[i]>nega_prob[j]) {cnt = cnt+1}
      else if(posi_prob[i]==nega_prob[j]) {cnt=cnt+0.5}
    }
  }
  return(round(cnt/total,7))
}

# trainx is a dataframe, trainy is a vector, k1 denotes k folds, k2 denote the k in knn.
# a list of mean accuracy and mean auc will be return.
knn_cv<-function(trainx,trainy,k1,k2){
  #set.seed(10)
  KFolds=createFolds(trainy,k=k1)
  accuracy=c()
  auc=c()
  for(i in 1:length(KFolds)){
    
    tr=trainx[-KFolds[[i]],]
    te=trainx[KFolds[[i]],]
    n=trainy[KFolds[[i]]]
    model=knn(train = tr,test = te,cl = trainy[-KFolds[[i]]],k=k2,prob = T)
    accuracy[i]=sum(model==trainy[KFolds[[i]]])/length(n)
    da=data.frame(trainy[KFolds[[i]]],model,attributes(model)$prob)
    names(da)=c('y','pred_y','prob')
    da$prob1<-ifelse(da$pred_y==1,da$prob,1-da$prob)
    auc[i]=roc(da$y,da$prob1)$auc[1]
    
  }
  result=list(k2,accuracy,mean(accuracy),auc,mean(auc))
  names(result)=c('k-neighbor','Accuracy Score','Avearge Accuracy Score','AUC','Avearge AUC')
  return(result)
}
find_knn_bestk<-function(start,end,trainx,trainy,k1){
  all_li=list()
  for(i in start:end){
    all_li=c(all_li,knn_cv(trainx,trainy,k1,i))
  }
  j=1
  acc<-c()
  for(i in seq(3,length(all_li),5)){
    acc[j]=all_li[[i]]
    j=j+1
  }
  m=1
  auc<-c()
  for(i in seq(5,length(all_li),5)){
    auc[m]=all_li[[i]]
  }
  for(i in seq(3,length(all_li),5)){
    if(all_li[[i]]==max(acc)){
      k_acc=all_li[[i-2]]
      break
    }
  }
  for(i in seq(5,length(all_li),5)){
    if(all_li[[i]]==max(auc)){
      k_auc=all_li[[i-4]]
      break
    }
  }
  li=list(paste(k_acc,':',max(acc)),paste(k_auc,':',max(auc)),'All Results')
  names(li)=c('Best k suggested by accuracy score','Best k suggested by auc score','--------------------------------')
  return(c(li,all_li))
}

#two asset portfolio
Portfolio2<-function(mu1,mu2,sigma1,sigma2,p){
  a=sigma1^2-2*p*sigma1*sigma2+sigma2^2
  b=mu2*sigma1^2-(mu1+mu2)*p*sigma1*sigma2+mu1*sigma2^2
  c=mu2^2*sigma1^2-2*p*mu1*mu2*sigma1*sigma2+mu1^2*sigma2^2
  mu=b/a
  sigma=sqrt((c-b^2/a)/(mu1-mu2)^2)
  alpha1=(mu-mu2)/(mu1-mu2)
  alpha2=1-alpha1
  weight=matrix(c(alpha1,alpha2),byrow = T,ncol = 2)
  colnames(weight)=c('asset1','asset2')
  name='The details are give below:'
  res=list(name,mu,sigma,weight)
  names(res)=c('Minimum Variance Portfolio (MVP) ','portfolio mean','portfolio sd','weight')
  return(res)
}

#N assets portfolio
#targetmu:目标平均收益
#mu:股票期望向量
#sigma:股票协方差矩阵
Portfolion=function(targetmu,mu,sigma){
  one=matrix(1,nrow = length(mu),ncol = 1)
  sigmaInv=solve(sigma)
  A=as.numeric(t(mu)%*%sigmaInv%*%one)
  B=as.numeric(t(mu)%*%sigmaInv%*%mu)
  C=as.numeric(t(one)%*%sigmaInv%*%one)
  D=B*C-A^2
  targetw=(B*sigmaInv%*%one-A*sigmaInv%*%mu+targetmu*(C*sigmaInv%*%mu-A*sigmaInv%*%one))/D
  targetVar=as.numeric(t(targetw)%*%sigma%*%targetw)
  MVPmu=A/C
  MVPVar=1/C
  MVPw=(sigmaInv%*%one)/C
  res=c(list(targetmu),list(targetw),list(targetVar),list(MVPmu),list(MVPw),list(MVPVar))
  names(res)=c('TargetMu',"MostEfficientPortfolio",'TargetVar','MVPMu','MVP','MVPVar')
  return(res)
}

#portfolio with risk-free asset
PortfolioRF=function(mu0,mu1,mu2,sigma1,sigma2,p,targetmu){
  a=(mu1-mu0)*sigma2^2-(mu2-mu0)*p*sigma1*sigma2
  b=(mu1-mu0)*sigma2^2+(mu2-mu0)*sigma1^2-(mu1-mu0+mu2-mu0)*p*sigma1*sigma2
  y=a/b
  muT=y*mu1+(1-y)*mu2
  sigmaT=sqrt(y^2*sigma1^2+2*p*sigma1*sigma2*y*(1-y)+(1-y)^2*sigma2^2)
  alpha=(muT-targetmu)/(muT-mu0)
  weight=matrix(c(alpha,(1-alpha)*y,(1-alpha)*(1-y)),ncol = 3)
  colnames(weight)=c('Risk.free','Risk1','Risk2')
  result=c(list(y),list(muT),list(sigmaT),list(alpha),list(weight))
  names(result)=c('y.t','mu.T','sigma.T','alpha','weight')
  return(result)
}

PortfolioRFPlot<-function(mu0,mu1,mu2,sigma1,sigma2,p,targetmu){
  res=PortfolioRF(mu0,mu1,mu2,sigma1,sigma2,p,targetmu)
  res1=Portfolio2(mu1,mu2,sigma1,sigma2,p)
  sigma=seq(0,2*res$sigma.T,length.out=1000)
  mu.top=mu0+(res$mu.T-mu0)/res$sigma.T*sigma
  mu.bottom=mu0-(res$mu.T-mu0)/res$sigma.T*sigma
  mv=res1$`portfolio sd`
  sigmar=seq(mv,2*mv,length.out=1000)
  a=sigma1^2-2*p*sigma1*sigma2+sigma2^2
  b=mu2*sigma1^2-(mu1+mu2)*p*sigma1*sigma2+mu1*sigma2^2
  c=mu2^2*sigma1^2-2*p*mu1*mu2*sigma1*sigma2+mu1^2*sigma2^2
  mu_top=b/a+sqrt((mu1-mu2)^2/a*sigmar^2-c/a+b^2/a^2)
  mu_bottom=b/a-sqrt((mu1-mu2)^2/a*sigmar^2-c/a+b^2/a^2)
  p=ggplot()+
    geom_line(aes(x=sigma,y=mu.top,color='efficient frontier'))+
    geom_line(aes(x=sigma,y=mu.bottom))+
    geom_line(aes(x=sigmar,y=mu_top))+
    geom_line(aes(x=sigmar,y=mu_bottom))+
    geom_hline(yintercept = targetmu,linetype=3)+
    geom_ribbon(aes(x=sigma,ymin=mu.bottom,ymax=mu.top,fill='feasible region'),alpha=0.3)+
    geom_point(aes(x=res$sigma.T,y=res$mu.T),shape=17,size=2,color='red')+
    geom_point(aes(x=((1-res$weight[1])*res$sigma.T),y=targetmu),shape=18,size=2,color='orange')+
    geom_text(aes(x=res$sigma.T,y=1.2*res$mu.T),label='Tangency Portfolio',size=4,color='red')+
    geom_text(aes(x=((1-res$weight[1])*res$sigma.T),y=1.2*targetmu),label='MEP',size=4,color='orange')+
    scale_color_manual(values = c('efficient frontier'='#00FFFF'))+
    scale_fill_manual(values = c('feasible region'='lightpink'))+
    labs(y='mu')+
    theme(
      legend.title = element_blank(),
      legend.position = 'bottom'
    )
  return(p)
} 

#risk-free with n risks assets
EffPortfilio<-function(mu0,targetmu,mu,sigma){
  sigmaInv=solve(sigma)
  one=matrix(1,nrow = length(mu),ncol=1)
  top=(targetmu-mu0)*sigmaInv%*%(mu-mu0*one)
  bottom=t(mu-mu0*one)%*%sigmaInv%*%(mu-mu0*one)
  w=top/bottom[1]
  alpha=1-sum(w)
  w_m=w/sum(w)
  mep=matrix(c(alpha,w),nrow=1,ncol = length(c(alpha,w)))
  colnames(mep)=c('risk-free',rownames(w))
  res=c((list(mep)),list(alpha),list(t(w_m)))
  names(res)=c('Most.Efficient.Portfolio','alpha','Market.Portfolio')
  return(res)
}

#quantile regression of mse
mse_qr=function(tau,y,ypred){
  res=list()
  for(i in 1:length(tau)){
    mse=mean((y-ypred[,i])^2)
    res=c(res,mse)
  }
  res=c('--------------------',res)
  name=paste0('tau=',tau)
  names(res)=c('MSE',name)
  return(res)
}

#mae,mse,rmse
rerror=function(y,ypred,type){
  if(type=='mse'){
    result=mean((y-ypred)^2)
  }
  else if(type=='rmse'){
    result=sqrt(mean((y-ypred)^2))
  }
  else{
    result=mean(abs(y-ypred))
  }
  return(result)
}

# mar模型预测
# m:模型ar系数最大值
# model:fit_mixAR对象
# p:分位数
pred.mar=function(train,test,m,model,p){
  n=length(train)
  xcon=matrix(0,ncol = m,nrow = length(test))
  test1 = c(train[(n-m+1):n],test)
  for(i in 1:(length(test1)-m)){
    xcon[i,]=test1[i:(i+m-1)]
  }
  VaR=c()
  es=c()
  pred=c()
  for(i in 1:length(test)){
    mcdf=mix_cdf(model,xcond=xcon[i,])
    VaR[i]=gbutils::cdf2quantile(p,cdf=mcdf)
    sim=replicate(1000,mixAR_sim(model,n=1,init =xcon[i,],nskip = 0))
    pred[i]=mean(sim)
    es[i]=mean(sim[sim<=VaR[i]])
  }
  res = c(list(VaR),list(es),list(pred))
  names(res)=c('VaR','ES','Pred')
  return(res)
}

# GSMAR预测
# m:模型ar系数最大值
# model:gsmar对象
# p:分位数
pred.gsmar=function(train,test,m,model,p){
  n=length(train)
  xcon=matrix(0,ncol = m,nrow = length(test))
  test1 = c(train[(n-m+1):n],test)
  for(i in 1:(length(test1)-m)){
    xcon[i,]=test1[i:(i+m-1)]
  }
  VaR=c()
  es=c()
  pred=c()
  for(i in 1:length(test)){
    a=simulate(model,nsim = 1,init_values = xcon[i,],ntimes = 1000)$sample
    VaR[i]=quantile(a,p)
    es[i]=mean(a[a<VaR[i]])
    pred[i]=mean(a)
  }
  res = c(list(VaR),list(es),list(pred))
  names(res)=c('VaR','ES','Pred')
  return(res)
}

#非参数回归
local_average = function(x,y,h,newx){
  result = c()
  for(i in 1:length(newx)){
    result[i] = mean(y[which(abs(x-newx[i])<h)])
  }
  return(result)
}
# NW
NW = function(x,y,h,newx){
  result = c()
  for(i in 1:length(newx)){
    a = x-newx[i]
    bottom = sum(dnorm(a,sd = h))
    up = sum(dnorm(a,sd = h)*y)
    result[i]=up/bottom
  }
  return(result)
}

# local polynomial
local_poly = function(x,y,h,p,newx){
  result = c()
  for(i in 1:length(newx)){
    X = matrix(0,nrow = length(x),ncol = p+1)
    for(j in 0:p){
      X[,j+1] = (x-newx[i])^j
    }
    W = diag(dnorm(x-newx[i],sd = h))
    beta = solve(t(X)%*%W%*%X)%*%t(X)%*%W%*%as.matrix(y)
    result[i] = beta[1]
  }
  return(result)
}

# 幸存者分析生存函数
survi = function(model,t){
  res = data.frame(model$time,model$surv)
  names(res) = c('time','surv')
  result = c()
  for(i in 1:length(t)){
    if(t[i] < min(res$time)){
      result[i] = 1
    }
    else if(t[i] > max(res$time)){
      result[i] = 0
    }
    else{
      result[i] = min(res[res$time <= t[i],2])
    }
  }
  return(result)
}

# ks曲线
ks.plot = function(da){
  names(da)=c('y','y_pred,','prob')
  roc_obj1=roc(da$y,da$prob)
  TPR=roc_obj1$sensitivities
  FPR=1-roc_obj1$specificities
  threshold=roc_obj1$thresholds
  data1=data.frame(TPR,FPR,threshold)
  max_thre=data1[which(data1$TPR-data1$FPR==max(data1$TPR-data1$FPR)),][3][1,1]
  tpr=data1[which(data1$TPR-data1$FPR==max(data1$TPR-data1$FPR)),][1][1,1]
  fpr=data1[which(data1$TPR-data1$FPR==max(data1$TPR-data1$FPR)),][2][1,1]
  p = ggplot(data1,aes(x=threshold))+
    geom_line(aes(y=TPR,color='TPR'),linewidth=0.8)+
    geom_line(aes(y=FPR,color='FPR'),linewidth=0.8)+
    geom_line(aes(y=TPR-FPR,color='KS'),linewidth=0.8)+
    geom_segment(
      aes(x=max_thre,xend=max_thre,
          y=fpr,yend=tpr),linetype=2
    )+
    xlim(c(1,0))+
    scale_color_manual(values = c('TPR'='orange','FPR'='skyblue',
                                  'KS'='lightgreen'))+
    ggtitle('KS Curve')+
    theme(
      legend.title = element_blank(),
      plot.title = element_text(hjust=0.5)
    )
  return(p)
}

# roc曲线
roc.plot = function(da){
  names(da)=c('y','y_pred,','prob')
  roc_obj=roc(da$y,da$prob,smooth=T)
  TPR=roc_obj$sensitivities
  FPR=1-roc_obj$specificities
  data=data.frame(TPR,FPR)
  p = ggplot(data,aes(FPR,TPR))+
    geom_line(color='skyblue',linewidth=0.8)+
    geom_segment(x=0,y=0,xend=1,yend=1,linetype=2)+
    ggtitle('ROC Curve')+
    theme(
      plot.title = element_text(hjust=0.5)
    )
  return(p)
}













