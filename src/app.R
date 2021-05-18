library(rsconnect)
deployApp()

source("ui.R")
source("server.R")
source("global.R")
source("Ameisenalgorithmus_Implementierung_2.R")



#run the App
shinyApp(ui = ui, server = server)
