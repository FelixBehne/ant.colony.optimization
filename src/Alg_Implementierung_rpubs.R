
#https://rpubs.com/gingersling/ACO
#initialize the ants
randParam<-function(paramList,hor){
  res<-data.frame(param=numeric())
  for(i in 1:hor){
    for(j in 1:dim(paramList)[2])
      res[i,j]<-runif(1,paramList[1,j],paramList[2,j])
  }
  res
}

#calculate the errors
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

#calculate the weights
pesos<-function(err,q){
  tot<-dim(err)[1]
  res<-data.frame(w=numeric())
  for(i in 1:dim(err)[1]){
    res[i,]<-(1/(q*tot*sqrt(2*pi)))*exp(-(which(err[i,]==err[order(err),])[1]-1)^(2)/(2*(q*tot)^(2)))
  }
  res
}

#calculate the probability
probHor<-function(peso){
  res<-peso
  tot<-sum(peso[,1])
  res<-res/tot
  res
  
}

#calculate the sigma fot each variable
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

#compute new generation
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

#Hauptmethode
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

#--------------------------------------------
#Rosenbrock-Funktion graphisch darstellen
rosenbrock <- function(x, y){
  (1-x)^2+100*(y-x^2)^2
}
x <- y <- seq(-1, 1, length= 20)
z <- outer(x, y, rosenbrock)

persp(x, y, z,
      main="Rosenbrock Funktion (a=1, b=100)",
      theta = 150, phi = 20,
      col = "green", shade = 0.4)

#--------------------------------------------
## Rosenbrock Banana function Mimimum berechnen
fr <- function(x) {   
  x1 <- x[1]
  x2 <- x[2]
  100 * (x2 - x1 * x1)^2 + (1 - x1)^2
}
grr <- function(x) { ## Gradient of Rosenbrock Banana function
  x1 <- x[1]
  x2 <- x[2]
  c(-400 * x1 * (x2 - x1^2) - 2 * (1 - x1), #erste Ableitung nach x1
    200 *      (x2 - x1^2)) # erste Ableitung nach x2
}
print(optim(c(-1.2,1), fr)) # convergence=0, wenn Funktion konvergiert hat; Mimimum bei x1=1, x2=1, f=0 
#-------------------------------------------

#Ant Optim. Alg auf Rosenbrock Function anwenden
costFRosenbrock <- function(datos,paramList){
  x1<-paramList[1]
  x2<-paramList[2]
  100 * (x2 - x1 * x1)^2 + (1 - x1)^2
}
set.seed(120)
#vars<-data.frame(x1=c(-5,5),x2=c(-5,5))# Inter von x1, x2
#ACO(datos="NA",costF=costFRosenbrock,paramListR=vars,hor=100,q=0.7,eps=0.5,gen=1000,tip=c("num","num"),paralelo=0,printIt=200)
#--> konvergiert nicht nach 1000 iterationen

#-------------------------------------------
#library(knitr)    # For knitting document and include_graphics function
#library(ggplot2)  # For plotting
#library(png)  
#img1_path <- "images/ant.png"
#img1 <- readPNG(img1_path, native = TRUE, info = TRUE)
#attr(img1, "info")
#-------------------------------------------
#Himmelblau-Funktion graphisch darstellen
himmelblau <- function(x, y){
  (x^2+y-11)^2+(x+y^2-7)^2
}
x <- y <- seq(-5, 5, length= 20)
z <- outer(x, y, himmelblau)

persp(x, y, z,
      main="Himmelblau Funktion",
      theta = -40, phi = 50,
      col = "orange", shade = 0.5)

#--------------------------------------------
## Himmelblaufunktion Mimimum berechnen
f_himmelblau <- function(x) {   
  x1 <- x[1]
  x2 <- x[2]
  (x1^2+x2-11)^2+(x1+x2^2-7)^2
}
gradient_himmelblau <- function(x) { ## Gradient der Funktion
  x1 <- x[1]
  x2 <- x[2]
  c(4*x1*(x1^2+x2-11)+2(x1+x2^2 − 7) ,
    4*x2*(x2^2+x1-7)+2*(x2+x1^2-11))
}
print(optim(c(-5,5), f_himmelblau)) # convergence=0, wenn Funktion konvergiert hat; 4 lokale Mimima, globales Minimum bei x1=-3.77, x2=-3.28, f=0 
#--> lokales Minimum wird gefunden

#----------------------------------
#Ant Optim. Alg auf Himmelblau Funktion anwenden
costFHImmelblau <- function(datos,paramList){
  x1<-paramList[1]
  x2<-paramList[2]
  (x1^2+x2-11)^2+(x1+x2^2-7)^2
}
set.seed(120)
vars<-data.frame(x1=c(-5,5),x2=c(-5,5))# Intervall von x1, x2
ACO(datos="NA",costF=costFHImmelblau,paramListR=vars,hor=100,q=0.7,eps=0.5,gen=800,tip=c("num","num"),paralelo=0,printIt=100)
#--> landet nach 300 Generationen in lokalem Minimum und kommt nicht mehr raus