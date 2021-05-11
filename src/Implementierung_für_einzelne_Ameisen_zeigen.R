library("ggplot2")

# set randomly first ants
randParam<-function(paramList,hor){
  res<-data.frame(param=numeric())
  for(i in 1:hor){
    for(j in 1:dim(paramList)[2])
      res[i,j]<-runif(1,paramList[1,j],paramList[2,j])
  }
  res
}
# calc error
calcErr<-function(datos,costF,paramList,paralelo){
  res<-data.frame(err=numeric())
  if(paralelo==0){
    coste<-match.fun(costF)
    tParam<-as.data.frame(t(paramList))
    aux<-unlist(lapply(tParam,function(x){coste(datos=datos,paramList=x)}))
    res<-as.data.frame(aux)
    res
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
    aux<-foreach(paramList1=tParam,.combine = c) %dopar% {
      library(caret)
      costF(datos=datos,paramList=paramList1)
    }
    stopCluster(cl)
    res<-as.data.frame(aux)
    res
  }
  res
}
#calc errors 
pesos<-function(err,q){
  tot<-dim(err)[1]
  res<-data.frame(w=numeric())
  for(i in 1:dim(err)[1]){
    res[i,]<-(1/(q*tot*sqrt(2*pi)))*exp(-(which(err[i,]==err[order(err),])[1]-1)^(2)/(2*(q*tot)^(2)))
  }
  res
}
# calc probability
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
# generate new Ants generation
newGen<-function(paramList,desv,prob,paramListR){
  res<-paramList
  hor<-dim(paramList)[1]
  pars<-dim(paramList)[2]
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

ACO<-function(datos,costF,paramListR,hor=50,q=0.2,eps=0.5,gen=20,tip,paralelo=0,printIt=1){
  genP<-randParam(paramListR,hor)
  meanErrP<-0.1
  bestHorP<-0.1
  while(gen>0){
    genP[,!( tip %in% "num")]<-round(genP[,!( tip %in% "num")])
    errGen<-calcErr(datos,costF,genP,paralelo=paralelo)
    if(((gen %% printIt) ==0) | (gen == 1)){
      cat(as.character(Sys.time()),append = TRUE,sep = "\n")
      cat(paste("Generation",gen),append = TRUE,sep = "\n")
      cat(paste("Mean error of: ",sum(errGen)/hor),append = TRUE,sep = "\n")
      cat(paste("Mean value for the varaibles are: ",colMeans(genP)),append = TRUE,sep = "\n")
      cat(paste("Best ant's values: ",genP[order(errGen)[1],]),append = TRUE,sep = "\n")
      cat(paste("Best ant's error: ",min(errGen)),append = TRUE,sep = "\n")
    }
    pesosGen<-pesos(errGen,q)
    probGen<-probHor(pesosGen)
    desv<-cSigma(genP,eps)
    genP<-newGen(genP,desv,probGen,paramListR)
    gen<-gen-1
  }
}

#----------------------
costFHImmelblau <- function(datos,paramList){
  x1<-paramList[1]
  x2<-paramList[2]
  (x1^2+x2-11)^2+(x1+x2^2-7)^2
}
#----------------------

makeStartSet<-function(numberOfAnts, anfangsintervall){
set.seed(120)
genP = randParam(paramList=anfangsintervall, hor=numberOfAnts)
#genP_df <- data.frame(matrix(unlist(genP), nrow=length(genP), byrow=TRUE))
#genP_matrix <- matrix(c(genP[1]),c(genP[2]))
#genP_matrix <- matrix(c(c(genP[1]),c(genP[2])), ncol = 2, nrow =numberOfAnts)
return(genP)
}
# Anfangsintervall
anfangsintervall<-data.frame(x1=c(-10,0),x2=c(-6.5,0))
#anfangsintervall<-data.frame(x1=c(-10,0),x2=c(-10,0))
generation = makeStartSet(100, anfangsintervall)
print(generation)
#plot(generation)
#par(mar = rep(2, 4))
xAchseLänge=nrow(generation)+1
image(x=xAchseLänge, y=xAchseLänge, z=generation,col=cx,axes=T)



errGen<-calcErr(datos="NA",costF=costFHImmelblau,paramList=genP,paralelo=0)
#plot(unlist(errGen))
#calc weights
pesosGen<-pesos(err=errGen,q=0.2)
#clac probability
probGen<-probHor(pesosGen)
desv<-cSigma(genP,eps=0.5)
genP<-newGen(paramList=genP,desv=desv,prob=probGen,paramListR=vars)
print(genP)
plot(unlist(genP))