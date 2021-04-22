library(shiny)

#source("Alg_Implementierung_rpubs.R")
source("ui.R")
source("server.R")


#run the App
shinyApp(ui = ui, server = server)