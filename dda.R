covariance<-function(x,y){
  data=cbind(x,y)
  type=unique(data[,length(names(data))])
  dim=length(names(data))
  s=matrix(0,nrow =dim-1,ncol = dim-1 )
  for(i in 1:length(type)){
    da=data[data[,length(names(data))]==type[i],-length(names(data))]
    s=s+stats::cov(da)*(length(da[,1])-1)
  }
  cov=s/(length(data[,1])-length(type))
  return(cov)
}

dda<-function(x,y,xpred,sigma){
  data=cbind(x,y)
  type=unique(data[,length(names(data))])
  dim=length(names(data))
  md=c()
  for(i in 1:length(type)){
    da=data[data[,length(names(data))]==type[i],-length(names(data))]
    x_bar=as.matrix(apply(da,2,mean))
    xpred1=t(as.matrix(xpred))
    md[i]=t(xpred1-x_bar)%*%solve(sigma)%*%(xpred1-x_bar)
  }
  cl=type[which(md==min(md))]
  return(cl)
}

#x,xp为dataframe,y为vector.
dda.p<-function(x,y,xp){
  sigma=covariance(x,y)
  l=length(xp[,1])
  result=c()
  for(i in 1:l){
    result[i]=dda(x,y,xp[i,],sigma)
  }
  res=list(pred_y=result)
  return(res)
}