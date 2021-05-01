library(shiny)
library(DT)



#source("Alg_Implementierung_rpubs.R")
source("ui.R")
source("server.R")
source("global.R")


#run the App
shinyApp(ui = ui, server = server)