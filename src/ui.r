#https://shiny.rstudio.com/gallery/navbar-example.html
library(markdown)
library(shinydashboard)
fluidPage(
  # App title ----
  titlePanel("Ant Optimization Algorithm"),
  theme=shinythemes::shinytheme("yeti"),
      tabsetPanel(
           tabPanel("Generelles",
                    fluidRow(
                      column(12,
                             includeMarkdown("generalInfo.Rmd")
                      ))),   
        navbarMenu("Implementierung",
           tabPanel("Plot Rosenbrock",
                    titlePanel("Minimierung der Rosenbrock Funktion"),
                    fluidRow(
                      column(4,
                    #sidebarLayout(
                     # sidebarPanel(
                          title = "Inputs Optimierungsfunktion", status = "warning", solidHeader = TRUE,
                          textOutput("textOne"),
                          br(),
                          #radioButtons("functione", "Zu minimierende Funktion: ",
                          #             c("Rosenbrock"="rosenbrock", "Himmelblau"="himmelblau"))
                          #sliderInput("sliderX","Min and max X:",
                          #              min = -100,max = 100,value = c(-25,25)),
                          #sliderInput("sliderY","Min and max Y",
                          #                 min = -100,max = 100,value = c(-25,25)),
                          #sliderInput("sliderLtheta","Ltheta",
                          #                 min = -180,max = 180,value = -120)
                          
                          sliderInput("phiR","Phi Rotation:",
                                       min = 1,max = 300,value = 20),
                          sliderInput("thetaR","Theta Rotation:",
                                      min = 1,max = 300,value = 150),
                          sliderInput("shadeR", "Schattierung:",
                                      min = 0, max = 1, value = 0.3),
                          numericInput("intervalMinR","Intervall Untergrenze",value=-1,
                                        min=-50,max=-1,step=1),
                          numericInput("intervalMaxR","Intervall Obergrenze",value=1,
                                     min=1,max=50,step=1)
                      ),
          
                          #DT::dataTableOutput("textAntAlg")
                      #mainPanel(
                    column(6,
                      plotOutput("plotOne"),
                      textOutput('minRoseText'),
                      tableOutput('tableMinimaRose'))),
                      
                   
                      fluidRow(
                        column(12, style = "background-color:	#E8E8E8;",
                        br(),
                        textOutput('textAntRose'),
                        sliderInput("iterationsR","Iterationen",
                                  min = 0,max = 120,value = 1,step=1),
                        shinycssloaders::withSpinner(tableOutput('tableAntRose')))
                      )),
            tabPanel("Plot Himmelblau",
                     titlePanel("Anwendung auf die Himmelblau Funktion"),
                     sidebarLayout(
                       sidebarPanel(
                         textOutput("textTwo"),   
                       sliderInput("phiH","Phi rotation:",
                                   min = 1,max = 300,value = 50),
                       sliderInput("thetaH","Theta rotation:",
                                   min = -40,max = 300,value = -40),
                       numericInput("intervalMinH","Intervall Untergrenze",value=-5,
                                    min=-50,max=-1,step=1),
                       numericInput("intervalMaxH","Intervall Obergrenze",value=5,
                                    min=1,max=50,step=1),
                       textOutput('minHimText'),
                       tableOutput('tableMinimaHim'),
                       textOutput('textAntHim'),
                       sliderInput("iterationsH","Iterationen",
                                   min = 0,max = 120,value = 1,step=1),
                       shinycssloaders::withSpinner(tableOutput('tableAntHim')))
                     ,
                       mainPanel(
                         plotOutput("plotTwo"))
                       ))),
        tabPanel("Taveling salesman",
                      verbatimTextOutput("travel")
                     ),
        tabPanel("Performance",
                 verbatimTextOutput("perform")
                 ),
       navbarMenu("More",
                   tabPanel("Hinweise zur R-Shiny Erstellung"
                            ),
                   tabPanel("Sonstiges",
                                     verbatimTextOutput("about")
                            )
                 )
      )  )
     
           
                 