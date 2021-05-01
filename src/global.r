library(shinycssloaders)

#Rosenbrock Funktion Minimum berechnen 
f_rosenbrock <- function(x) {   
  x1 <- x[1]
  x2 <- x[2]
  100 * (x2 - x1 * x1)^2 + (1 - x1)^2
}
grr <- function(x) { ## Gradient der Funktion
  x1 <- x[1]
  x2 <- x[2]
  c(-400 * x1 * (x2 - x1^2) - 2 * (1 - x1), #erste Ableitung nach x1
    200 *      (x2 - x1^2)) # erste Ableitung nach x2
}
# Berechnet Minimum: wenn convergence=0 Funktion konvergiert; Mimimum bei x1=1, x2=1, f=0 
Optimum = optim(c(-1.2,1), f_rosenbrock)

#für Ausgabe
MinimaRosenbrock <- data.frame(
  x1 = c(Optimum$par[1]),
  x2 = c(Optimum$par[2]),
  f = c (Optimum$value)
)
#----------------------------------------------------
#Himmelblau Funktion Minimum berechnen

f_himmelblau <- function(x) {   
  x1 <- x[1]
  x2 <- x[2]
  (x1^2+x2-11)^2+(x1+x2^2-7)^2
}
gradient_himmelblau <- function(x) { ## Gradient der Funktion
  x1 <- x[1]
  x2 <- x[2]
  c(4*x1*(x1^2+x2-11)+2(x1+x2^2-7),
    4*x2*(x2^2+x1-7)+2*(x2+x1^2-11))
}
OptimumHim = optim(c(-5,5), f_himmelblau) # convergence=0, wenn Funktion konvergiert hat; 4 lokale Mimima, globales Minimum bei x1=-3.77, x2=-3.28, f=0 

#weitere Minima hinzufügen
MinimaHimmelblau <-data.frame(
  x1 = c(OptimumHim$par[1], 3, -3.78, 3.58),
  x2 = c(OptimumHim$par[2], 2, -3.28, -1.85),
  f = c (OptimumHim$value, 0, 0, 0)
)
#-----------------------------------------------------
#Ant Optimization Algorythmus Implementierung
#Quelle:  https://rpubs.com/gingersling/ACO
#1. Initialisierung der Ameisen
randParam<-function(paramList,hor){
  res<-data.frame(param=numeric())
  for(i in 1:hor){
    for(j in 1:dim(paramList)[2])
      res[i,j]<-runif(1,paramList[1,j],paramList[2,j])
  }
  res
}

#2. Berechnen der Fehler
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

#3. Gewichtung berechnen
pesos<-function(err,q){
  tot<-dim(err)[1]
  res<-data.frame(w=numeric())
  for(i in 1:dim(err)[1]){
    res[i,]<-(1/(q*tot*sqrt(2*pi)))*exp(-(which(err[i,]==err[order(err),])[1]-1)^(2)/(2*(q*tot)^(2)))
  }
  res
}

#4. Wahrscheinlichkeit berechnen
probHor<-function(peso){
  res<-peso
  tot<-sum(peso[,1])
  res<-res/tot
  res
}

#5. Sigma für jede Variable berechnen
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

#Neue Generation berechnen
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

#Hauptmethode, in die eine Kostenfunktion eingegetzt werden kann
AntOptimization<-function(datos,costF,paramListR,hor=50,q=0.2,eps=0.5,gen=20,tip,paralelo=0,printIt=1){
  genP<-randParam(paramListR,hor)
  meanErrP<-0.1
  bestHorP<-0.1
  while(gen>0){
    genP[,!( tip %in% "num")]<-round(genP[,!( tip %in% "num")])
    errGen<-calcErr(datos,costF,genP,paralelo=paralelo)
    
   # if(((gen %% printIt) ==0) | (gen == 1)){
      #daten in form von Dataframe speichern
     # ergebnisGen <- data.frame(
     #   generation = c(gen), 
     #   f = c(sum(errGen)/hor),
     #   X_Werte = c(colMeans(genP))
     # )
    #  finalErg <- rbind(ergebnisGen)
      
    #  cat(as.character(Sys.time()),append = TRUE,sep = "\n")
    #  cat(paste("Generation",gen),append = TRUE,sep = "\n")
    #  cat(paste("Mean error of: ",sum(errGen)/hor),append = TRUE,sep = "\n")
    #  cat(paste("Mean value for the variables are: ",colMeans(genP)),append = TRUE,sep = "\n")
    #  cat(paste("Best ant's values: ",genP[order(errGen)[1],]),append = TRUE,sep = "\n")
    #  cat(paste("Best ant's error: ",min(errGen)),append = TRUE,sep = "\n")
   # }
    pesosGen<-pesos(errGen,q)
    probGen<-probHor(pesosGen)
    desv<-cSigma(genP,eps)
    genP<-newGen(genP,desv,probGen,paramListR)
    gen<-gen-1
  }
  #return( list(generation = gen, meanError = sum(errGen)/hor, MeanValueOfVariables = colMeans(genP)) )
  return( list(meanError = sum(errGen)/hor, Mean_x1 = mean(genP[[1]]), Mean_x2 = mean(genP[[2]])))
}
#--------------------------------------------
#Implementierten Ant Optim. Alg auf Rosenbrock Funktion anwenden
costFRosenbrock <- function(datos,paramList){
  x1<-paramList[1]
  x2<-paramList[2]
  100 * (x2 - x1 * x1)^2 + (1 - x1)^2
}
#set.seed(120)
#vars<-data.frame(x1=c(-5,5),x2=c(-5,5))# Intervallbereich von x1, x2
#antOptimRosenbrock = AntOptimization(datos="NA",costF=costFRosenbrock,paramListR=(data.frame(x1=c(-5,5),x2=c(-5,5))),hor=100,q=0.7,eps=0.5,gen=500,tip=c("num","num"),paralelo=0,printIt=100)
#--> konvergiert nicht nach 1000 iterationen
#---------------------------------------------

#Ant Optimization Algorythm mit Package Evoper
library(evoper)

calculateMin <- function(iter=30,minim=-1,maxim=1,fu ='rosenbrock'){ 
set.seed(161803398)
  if (fu == 'rosenbrock'){
    rosenbrock2<-function(x1,x2){(1-x1)^2+100*(x2-x1^2)^2}
    objective<-PlainFunction$new(rosenbrock2)
    }
  else if (fu == 'himmelblau'){
    himmelblau2<-function(x1,x2){(x1^2+x2-11)^2+(x1+x2^2-7)^2}
    objective<-PlainFunction$new(himmelblau2)
  }

objective$Parameter(name="x1",min=minim,max=maxim )
objective$Parameter(name="x2",min=minim,max=maxim )

acorOptions<-OptionsACOR$new ( )
acorOptions$setValue("iterations",iter)
system.time(results<-extremize("acor", objective, acorOptions))
#results$getBest()
antOptimRosenbrock <- data.frame(
  x1 = c (results$getBest()$x1), 
  x2 = c(results$getBest()$x2),
  f = c(results$getBest()$fitness)
)
return(antOptimRosenbrock)      
} 

