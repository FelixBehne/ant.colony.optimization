library(shiny)
library(DT)
library(markdown)
library(shinydashboard)
library(shinycssloaders)
library(ggplot2)
library(shinyWidgets)
library(plotly)

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

#---Plot Generations of ants on Himmelblau function --------------------

costFHImmelblau <- function(datos,paramList){
  x1<-paramList[1]
  x2<-paramList[2]
  (x1^2+x2-11)^2+(x1+x2^2-7)^2
}

#Intervall von x und y 
#vars<-data.frame(x1=c(-10,10),x2=c(-10,10))

#Anzahl Ameisen
#horNumb = 50

makeStartSet<-function(numberOfAnts, anfangsintervall){
  set.seed(120)
  genP = randParam(paramList=anfangsintervall, hor=numberOfAnts)
  genP_df <- data.frame(matrix(unlist(genP), nrow=length(genP), byrow=TRUE))
  #genP_matrix <- matrix(c(genP[1]),c(genP[2]))
  #genP_matrix <- matrix(c(c(genP[1]),c(genP[2])), ncol = 2, nrow =numberOfAnts)
  return(genP)
}
getFirstGenerationWithF<-function(datos="NA", genP, costF, paralelo=0){
  errGen<-calcErr(datos,costF,genP,paralelo=paralelo)
  XYF <- c(genP, errGen)
  return(XYF)
}


firstGeneration = makeStartSet(numberOfAnts = horNumb, anfangsintervall = vars) # gibt numberOfAnts-viele paare von zufälligen Werten im Anfangsbereich zurück
print(firstGeneration)
vekGen= c(firstGeneration)
print(vekGen)
print(data.frame(vekGen))

#Values for first generation, use Algorythm from "Versuch 2.r" 
ACO_calcGens<-function(datos="NA",costF,paramListR,genP,gen,q=0.2,eps=0.5,paralelo=0){
  meanErrP<-0.1
  bestHorP<-0.1
  while(gen>0){
    errGen<-calcErr(datos,costF,genP,paralelo=paralelo)
    #if(((gen %% printIt) ==0) | (gen == 1)){
    #plot(genP)
    #}
    pesosGen<-pesos(errGen,q)
    probGen<-probHor(pesosGen)
    desv<-cSigma(genP,eps)
    genP<-newGen(genP,desv,probGen,paramListR)
    gen<-gen-1
  }
  XYF <- c(genP, errGen)
  return(XYF)
}

prepareForPlot<-function(horNumb, xyf){ #Anzahl Ameisen und xyz-Werte
  
  #Spalte für Farbe hinzufügen   
  vektorForColour <- rep('ants', horNumb) # vektor mit länge hor (Ameisenanzahl) und einem gleichen Wert
  xyf$colour<- vektorForColour 
  
  #add first minimum of Himmelblau
  xyf$x <- c(xyf$x, -2.80)
  xyf$y <- c(xyf$y, 3.13)
  xyf$f <- c(xyf$f, 0.00)
  xyf$colour <- c(xyf$colour, 'min')
  
  #add second minimum of Himmelblau
  xyf$x <- c(xyf$x, 3.00)
  xyf$y <- c(xyf$y, 2.00)
  xyf$f <- c(xyf$f, 0.00)
  xyf$colour <- c(xyf$colour, 'min')
  
  #add third minimum of Himmelblau
  xyf$x <- c(xyf$x, -3.78)
  xyf$y <- c(xyf$y, -3.28)
  xyf$f <- c(xyf$f, 0.00)
  xyf$colour <- c(xyf$colour, 'min')
  
  #add fourth minimum of Himmelblau
  xyf$x <- c(xyf$x, 3.58)
  xyf$y <- c(xyf$y, -1.85)
  xyf$f <- c(xyf$f, 0.00)
  xyf$colour <- c(xyf$colour, 'min')
  
  # add mean values of ants
  meanF= sum(xyf$f)/horNumb
  meanX= sum(xyf$x)/horNumb
  meanY= sum(xyf$y)/horNumb
  print(typeof(meanF))
  
  #add mean values of ants
  xyf$x <- c(xyf$x, meanX)
  xyf$y <- c(xyf$y, meanY)
  xyf$f <- c(xyf$f, meanF)
  xyf$colour <- c(xyf$colour, 'mean')
  
  return(xyf)
}

#------------------------------------------
#https://www.r-bloggers.com/2018/06/valuebox-without-shinydashboard-2/

valueBoxExample <- function(value, subtitle, icon, color) {
  div(class = "col-lg-3 col-md-6",
      div(class = "panel panel-primary",
          div(class = "panel-heading", style = paste0("background-color:", color),
              div(class = "row",
                  div(class = "col-xs-3",
                      icon(icon, "fa-5x")
                  ),
                  div(class = ("col-xs-9 text-right"),
                      div(style = ("font-size: 56px; font-weight: bold;"),
                          textOutput(value)
                      ),
                      div(subtitle)
                  )
              )
          ),
          div(class = "panel-footer",
              div(class = "clearfix")
          )
      )
  )
}
