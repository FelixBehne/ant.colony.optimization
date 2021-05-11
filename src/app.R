library(rsconnect)
deployApp()

source("ui.R")
source("server.R")
source("global.R")



#run the App
shinyApp(ui = ui, server = server)
