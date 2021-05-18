#Implementierung Alg und für Plotten von Ameisengenerationen
# Algorithmus von:
# https://rpubs.com/gingersling/ACO#:~:text=The%20algorithm%20is%20a%20method,the%20behavior%20of%20the%20ants.&text=From%20generation%20to%20generation%20the,weight%20based%20on%20different%20function.


# set randomly first ants
randParam<-function(paramList,hor){
  # res<-data.frame(x=numeric())
  res <- data.frame(matrix(ncol = 2, nrow = 0)) #erstelle result-Variable
  Spaltennamen <- c("x", "y") #ErgÃ¤nze Spaltennamen
  colnames(res) <- Spaltennamen 
  for(i in 1:hor){
    for(j in 1:dim(paramList)[2])
      res[i,j]<-runif(1,paramList[1,j],paramList[2,j])
  }
  res
}
#vars<-data.frame(x1=c(-5,5),x2=c(-5,5))# Intervallbreite von x1, x2
#testRandParam = randParam(paramList=vars, hor = 100)
#print(testRandParam)


# berechne Fnktionswert (Error)
calcErr<-function(datos,costF,paramList,paralelo){
  res<-data.frame(err=numeric())
  if(paralelo==0){
    coste<-match.fun(costF)
    tParam<-as.data.frame(t(paramList))
    f<-unlist(lapply(tParam,function(x){coste(datos=datos,paramList=x)}))
    res<-as.data.frame(f)
    return(res)
  }
  if(paralelo==1){
    library(foreach)
    library(doParallel)
    no_cores <- detectCores() - 1
    cl<-makeCluster(no_cores)
    coste<-match.fun(costF)
    tParam<-as.data.frame(t(paramList))
    cl <- makeCluster(no_cores)
    registerDoParallel(cl)
    f<-foreach(paramList1=tParam,.combine = c) %dopar% {
      library(caret)
      costF(datos=datos,paramList=paramList1)
    }
    stopCluster(cl)
    res<-as.data.frame(f)
    res
  }
  res
}
# berechne Gewichtungen
pesos<-function(err,q){
  tot<-dim(err)[1]
  res<-data.frame(w=numeric())
  for(i in 1:dim(err)[1]){
    res[i,]<-(1/(q*tot*sqrt(2*pi)))*exp(-(which(err[i,]==err[order(err),])[1]-1)^(2)/(2*(q*tot)^(2)))
  }
  res
}
# berechne Wahrscheinlichkeit
probHor<-function(peso){
  res<-peso
  tot<-sum(peso[,1])
  res<-res/tot
  res
}
# calc sigma
cSigma<-function(paramList,eps){
  hor<-dim(paramList)[1]
  res<-paramList
  for(i in 1:hor){
    for(j in 1:dim(paramList)[2]){
      des<-0
      for(h in 1:hor){
        des<-des+abs(paramList[i,j]-paramList[h,j])
      }
      res[i,j]<-eps*des/(hor-1)
    }
  }
  res
}
# berechne neue Ameisen-Generation
newGen<-function(paramList,desv,prob,paramListR){
  res<-paramList
  hor<-dim(paramList)[1] # Anzahl der x1 Werte
  pars<-dim(paramList)[2] # Anzahl der x2-Werte
  rNum<-matrix(abs(rnorm(hor*pars,1,0.1)),ncol=pars)
  auxR<-matrix(sample(c(-1,1),size=hor*pars,replace=TRUE),ncol=pars)
  rNum<-rNum*auxR
  for(i in 1:dim(paramList)[2]){
    idx<-sample(1:hor,size=hor,replace = TRUE,prob=prob[,1])
    res[,i]<-paramList[idx,i]+desv[idx,i]*rNum[,i]
    res[,i]<-ifelse(res[,i]<paramListR[1,i],paramListR[1,i],res[,i])
    res[,i]<-ifelse(res[,i]>paramListR[2,i],paramListR[2,i],res[,i])
  }
  res
}




