library(rsconnect)
deployApp()

source("ui.R")
source("server.R")
source("global.R")
source("AmeisenalgorithmusImplementierung.R")
source("TSP_Alg.R")


#run the App
shinyApp(ui = ui, server = server)
