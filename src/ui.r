

fluidPage(
  #  includeCSS("www/styles.css"),
  setBackgroundColor(color="F5F9FC"),
  useShinydashboard(),
  # App title ----
  fluidRow(
    column(9,
           titlePanel("Ant Colony Optimization")),
    column(1, offset = 1,
           img(src="ant.jpg", height = "75", weight = "125")
    )),
  br(),
  theme=shinythemes::shinytheme("yeti"),
  tabsetPanel(
    navbarMenu("Generelles",icon = icon("info"),
               tabPanel("Einordnung und Herkunft", style = "background-color:	#E8E8E8;",
                        includeMarkdown("generalInfo.Rmd")), 
               tabPanel("Ameisen bei der Futtersuche",
                        slickROutput("slickr",height= "534", width="1200"),
               ),         
               tabPanel("Übertragung auf Algorithmen",
                        titlePanel("Übertragung auf Algorithmen"),
                        br(),
                        h4('1. Berechne die bedingte Wahrscheinlichkeit, dass eine Ameise sich für einen bestimmten Weg entscheidet, 
                           ausgehend von ihrem aktuellen Standort'),
                        withMathJax(),
                        tags$head(
                          tags$style(
                            HTML(".MathJax {font-size: 4em !important;}"))
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
    
    navbarMenu("Visualisierung", icon = icon("eye"),
               tabPanel("Plot Rosenbrock",
                        titlePanel("Minimierung der Rosenbrock Funktion"),
                        fluidRow(
                          column(4,
                                 br(),
                                 sliderInput("phiR","Vertikale Rotation:",
                                             min = 1,max = 300,value = 20),
                                 sliderInput("thetaR","Horizontale Rotation:",
                                             min = 1,max = 300,value = 150),
                                 sliderInput("shadeR", "Schattierung:",
                                             min = 0, max = 1, value = 0.3)),
                          column(6,
                                 plotOutput("plotOne"),
                          )),
                        
                        fluidRow(style = "background-color:	#E8E8E8;",
                                 column(4, 
                                        numericInput("intervalMinR","Intervall Untergrenze",value=-1,
                                                     min=-50,max=-1,step=1),
                                        numericInput("intervalMaxR","Intervall Obergrenze",value=1,
                                                     min=1,max=50,step=1),
                                        sliderInput("iterationsR","Iterationen",
                                                    min = 0,max = 120,value = 1,step=1),
                                 ),
                                 column(3, offset = 1, 
                                        br(),
                                        textOutput('minRoseText'),
                                        br(),
                                        tableOutput('tableMinimaRose')),
                                 column(4,
                                        br(),
                                        textOutput('textAntRose'),
                                        br(),
                                        br(),
                                        shinycssloaders::withSpinner(tableOutput('tableAntRose')))
                        )),
               tabPanel("Plot Himmelblau",
                        titlePanel("Minimierung der Himmelblau Funktion"),
                        fluidRow(
                          column(4,
                                 br(),
                                 #textOutput("textTwo"),   
                                 sliderInput("phiH","Vertikale Rotation:",
                                             min = 1,max = 300,value = 50),
                                 sliderInput("thetaH","Horizontale Rotation:",
                                             min = -40,max = 300,value = -40),
                                 sliderInput("shadeH", "Schattierung:",
                                             min = 0, max = 1, value = 0.3),
                          ),
                          column(6,
                                 plotOutput("plotTwo")
                          )),
                        
                        fluidRow(style = "background-color:	#E8E8E8;",
                                 column(4, 
                                        numericInput("intervalMinH","Intervall Untergrenze",value=-5,
                                                     min=-50,max=-1,step=1),
                                        numericInput("intervalMaxH","Intervall Obergrenze",value=5,
                                                     min=1,max=50,step=1),
                                        
                                        sliderInput("iterationsH","Iterationen",
                                                    min = 0,max = 120,value = 1,step=1)),
                                 
                                 column(3, offset = 1,
                                        br(),
                                        textOutput('minHimText'),
                                        br(),
                                        tableOutput('tableMinimaHim')),
                                 column(4,
                                        br(),
                                        textOutput('textAntHim'),
                                        br(),
                                        br(),
                                        # table for the result of the algorithm after x generations 
                                        shinycssloaders::withSpinner(tableOutput('tableAntHim'))
                                 )
                        ),
                        # fluidRow(style = "background-color:	#E8E8E8;",
                        #       
                        #   column(4, 
                        #          textOutput('textAntHim'),
                        #          sliderInput("iterationsH","Iterationen",
                        #           min = 0,max = 120,value = 1,step=1)),
                        # 
                        #   column(2, 
                        #          br(),
                        #          valueBoxOutput("x_val_alg", width = '100%')),
                        #   column(2, 
                        #          br(),
                        #          valueBoxOutput("y_val_alg", width = '100%')),
                        #   column(2, 
                        #          br(),
                        #          valueBoxOutput("f_val_alg", width = '100%'))
                        #          
                        #         # shinycssloaders::withSpinner(tableOutput('tableAntHim'))
                        # )
               ),
               tabPanel("Plot Ameisen-Generationen",
                        titlePanel("Ameisengenerationen auf der Suche nach dem Minimum der Himmelblaufunktion"),
                        fluidRow(
                          column(4,
                                 
                                 br(),
                                 numericInput("uGrenze","Intervall Untergrenze",value=-5,
                                              min=-20,max=-1,step=1),
                                 numericInput("oGrenze","Intervall Obergrenze",value=5,
                                              min=1,max=20,step=1),
                                 sliderInput("horNumb","Anzahl Ameisen",
                                             min = 1,max = 100,value = 40),
                                 sliderInput("generationenAnzahl","Anzahl Generationen",
                                             min = 0,max = 50,value = 1),
                                 
                                 br(),
                                 h4('Minima der Himmelblaufunktion'),        
                                 tableOutput('tableMinimaHim2'),
                                 br(),
                                 actionButton("showGen",label= "Start", style='margin-left:10px; font-size:150%'),
                                 
                          ),
                          column(6,
                                 #h3("Himmelblau-Funktion als Kostenfunktion"),
                                 shinycssloaders::withSpinner(plotlyOutput("generation")),
                                 #   br(),
                                 #useShinydashboard(),
                                 #   fluidRow(
                                 #     column(6,
                                 # textOutput('generationNumber'),
                                 # tableOutput('tableMean'),
                                 # h5('mean x1: '),
                                 # textOutput('meanx'),
                                 # h5('mean x2: '),
                                 # textOutput('meany'),
                                 # h5('mean f: '),
                                 # textOutput('meanf')
                                 #     )
                                 #  )
                          )))),
    tabPanel("Taveling salesman", icon = icon("map-marked-alt"),
             verbatimTextOutput("travel"),
             titlePanel("Travelling Salesman Problem"),
             fluidRow(
               column(4,style = "background-color:	#add8e6;",
                      sliderInput("alpha","Alpha",min = 1,max = 10,value = 5),
                      sliderInput("beta","Beta",min = 1,max = 10,value = 5),
                      sliderInput("evaporation","Verdunstung ",min = 0.1,max = 1.0,value = 0.5),
                      sliderInput("randomnessf","Zufälligkeitsfaktor",min = 0,max = 10,value = 5),
                      sliderInput("nOfAnts","Anzahl der Ameisen",min = 1,max = 50,value = 30),
                      sliderInput("iterations","Iterationen",min = 1,max = 50,value = 30),
                      actionButton("action", "Action"),
                      actionButton('info', 'Info'),
                      br()
               ),
               column(8,
                      plotOutput("TSPlot"),
                      dataTableOutput('table'),
                      
               )
             ),
             fluidRow(style = "background-color:	#E8E8E8;",
               column(10,
                includeMarkdown("Anwendung.Rmd")
               )
             )
             
    ),
    tabPanel("Performance", icon = icon("chart-line"),
             verbatimTextOutput("perform")
    ),
    navbarMenu("More", icon= icon("hand-peace"),
               tabPanel("Hinweise zur R-Shiny Erstellung"
               ),
               tabPanel("Sonstiges",
                        verbatimTextOutput("about")
               )
    )
  )  
  
)   
