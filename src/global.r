library(shiny)
library(DT)
library(markdown)
library(shinydashboard)
library(shinycssloaders)
library("ggplot2")

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
#----------------------------------------------
returnPlot<- function(fu="rosenbrock",minim=-1,maxim=1, thetaRot = "150", phiRot= "20", shade = 0.3, colour="green"){ 
  
  if (fu == 'rosenbrock'){
    fun <- function(x, y){(1-x)^2+100*(y-x^2)^2}
    text = "Rosenbrock-Funktion mit a=1, b=100"
  }else if(fu == 'himmelblau'){
    fun <- function(x, y){(x^2+y-11)^2+(x+y^2-7)^2}
    text = "Himmelblau-Funktion"
  }
  x <- y <- seq(minim, maxim, length= 20)
  z <- outer(x, y, fun )
  persp(x, y, z, 
        main = text,
        theta = thetaRot, phi = phiRot,
        shade = shade, col = colour)
}
