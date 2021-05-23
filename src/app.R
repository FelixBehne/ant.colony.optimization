library(rsconnect)
deployApp()

source("ui.R")
source("server.R")
source("global.R")
source("AmeisenalgorithmusImplementierung.R")



#run the App
shinyApp(ui = ui, server = server)
