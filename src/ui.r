#https://shiny.rstudio.com/gallery/navbar-example.html

fluidPage(
  includeCSS("www/styles.css"),
  setBackgroundColor(color="ghostwhite"),
  useShinydashboard(),
  # App title ----
  titlePanel("Ant Colony Optimization"),
  theme=shinythemes::shinytheme("yeti"),
  tabsetPanel(
    navbarMenu("Generelles",
             tabPanel("Einordnung und Herkunft",
                     includeMarkdown("generalInfo.Rmd")), 
             tabPanel("Ameisen bei der Futtersuche",
                      includeMarkdown("ameisenFuttersuche.Rmd")),
             tabPanel("Übertragung auf Algorithmen",
                      titlePanel("Übertragung auf Algorithmen"),
                      br(),
                      h4('1. Berechne die bedingte Wahrscheinlichkeit, dass eine Ameise sich für einen bestimmten Weg entscheidet, ausgehend von ihrem aktuellen Standort'),
                      withMathJax(),
                      tags$head(
                        tags$style(
                          HTML(
                            ".MathJax {
                              font-size: 4em !important;
                              }"
                          )
                        )
                      ),
                      fluidRow(
                        column(10,
                      shinycssloaders::withSpinner(uiOutput('formel_one'))),
                        column(2,
                      actionButton("infobuttonFormel1",label= "" , width = '60px' , icon = icon("info"))
                        )),

                      h4('2. Berechne den neuen Pheromonwert nach partieller Verdunstung der alten Pheromone und Verteilung der neuen Pheromone'),
                      fluidRow(
                        column(10,
                        shinycssloaders::withSpinner(uiOutput('formel_two'))),
                        column(2,
                               actionButton("infobuttonFormel2",label= "" , width = '60px' , icon = icon("info"))
                        )),
                      h4('3. Belohnung mit Pheromonwerten'),
                      fluidRow(
                        column(10,
                      shinycssloaders::withSpinner(uiOutput('formel_three'))),
                      column(2,
                             actionButton("infobuttonFormel3",label= "" , width = '60px' , icon = icon("info"))
                      )))),
    
    navbarMenu("Implementierung",
               tabPanel("Plot Rosenbrock",
                        titlePanel("Minimierung der Rosenbrock Funktion"),
                        fluidRow(
                          column(4,
                                 #sidebarLayout(
                                 # sidebarPanel(
                                 # title = "Inputs Optimierungsfunktion", status = "warning", solidHeader = TRUE,
                                 textOutput("textOne"),
                                 br(),
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
                        fluidRow(
                          column(4,
                                 # sidebarLayout(
                                 #  sidebarPanel(
                                 textOutput("textTwo"),   
                                 sliderInput("phiH","Phi rotation:",
                                             min = 1,max = 300,value = 50),
                                 sliderInput("thetaH","Theta rotation:",
                                             min = -40,max = 300,value = -40),
                                 sliderInput("shadeH", "Schattierung:",
                                             min = 0, max = 1, value = 0.3),
                                 numericInput("intervalMinH","Intervall Untergrenze",value=-5,
                                              min=-50,max=-1,step=1),
                                 numericInput("intervalMaxH","Intervall Obergrenze",value=5,
                                              min=1,max=50,step=1)
                          ),
                          column(6,
                                 # mainPanel(
                                 plotOutput("plotTwo"),  
                                 textOutput('minHimText'),
                                 tableOutput('tableMinimaHim'))),
                        fluidRow(
                          column(12, style = "background-color:	#E8E8E8;",
                                 br(),
                                 
                                 textOutput('textAntHim'),
                                 sliderInput("iterationsH","Iterationen",
                                             min = 0,max = 120,value = 1,step=1),
                                 shinycssloaders::withSpinner(tableOutput('tableAntHim')))
                        )),
               tabPanel("Plot Ameisen-Generationen",
                        titlePanel("Generationen von Ameisen auf der Suche nach dem Minimum"),
                        fluidRow(
                          column(4,
                                 h3("Wähle Parameter:"),
                                 numericInput("uGrenze","Intervall Untergrenze",value=-5,
                                              min=-20,max=-1,step=1),
                                 numericInput("oGrenze","Intervall Obergrenze",value=5,
                                              min=1,max=20,step=1),
                                 sliderInput("horNumb","Anzahl Ameisen",
                                             min = 1,max = 100,value = 40),
                                 sliderInput("generationenAnzahl","Anzahl Generationen",
                                             min = 0,max = 50,value = 1),
                                 actionButton("showGen",label= "Start")
                                
                            ),
                          column(6,
                                 # mainPanel(
                                 h3("Himmelblau-Funktion als Kostenfunktion"),
                                 shinycssloaders::withSpinner(plotlyOutput("generation")),
                                 br(),
                                 #useShinydashboard(),
                                 fluidRow(
                                   column(12,
                                          
                                   h5('Generation: '),
                                   textOutput('generationNumber'),
                                   h5('mean x: '),
                                   textOutput('meanx'),
                                   h5('mean y: '),
                                   textOutput('meany'),
                                   h5('mean f: '),
                                   textOutput('meanf')
                                   )
                                )
                                 )))),
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
  )  
  
)   
