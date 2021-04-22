#https://shiny.rstudio.com/gallery/navbar-example.html
library(markdown)
navbarPage("Ant Optimization Algorythm",
           tabPanel("Generelles",
                    fluidRow(
                      column(6,
                             includeMarkdown("generalInfo.Rmd")
                      ))),   
        navbarMenu("Implementierung",
           tabPanel("Plot Rosenbrock",
                    sidebarLayout(
                      sidebarPanel(
                        
                        textOutput("textOne"),
                        #radioButtons("functione", "Zu minimierende Funktion: ",
                        #             c("Rosenbrock"="rosenbrock", "Himmelblau"="himmelblau"))
                        #numericInput("sliderLenght","Sequence lenght:",value=30,
                        #                          min=2,max=500,step=1)),
                        #sliderInput("sliderX","Min and max X:",
                        #              min = -100,max = 100,value = c(-25,25)),
                        #sliderInput("sliderY","Min and max Y",
                        #                 min = -100,max = 100,value = c(-25,25)),
                        #sliderInput("sliderLtheta","Ltheta",
                        #                 min = -180,max = 180,value = -120)
                        #sliderInput("sliderShade","Shade",
                        #          min = 0,max = 3,value = 0.75,step=0.1),
                        sliderInput("phiR","Phi rotation:",
                                     min = 1,max = 300,value = 20),
                         sliderInput("thetaR","Theta rotation:",
                                    min = 1,max = 300,value = 150)
                      ),
                      mainPanel(
                        plotOutput("plotOne")
                      )
            )
        ),
            tabPanel("Plot Himmelblau",
                     sidebarLayout(
                       sidebarPanel(
                         textOutput("textTwo"),   
                       sliderInput("phiH","Phi rotation:",
                                   min = 1,max = 300,value = 50),
                       sliderInput("thetaH","Theta rotation:",
                                   min = -40,max = 300,value = -40) 
                     ),
                       mainPanel(
                         plotOutput("plotTwo")
                       ))
                     )), 
        tabPanel("Taveling salesman",
                      verbatimTextOutput("travel")
                     ),
        tabPanel("Performance",
                 verbatimTextOutput("perform")
                 ),
       navbarMenu("More",
                   tabPanel("Hinweise zur R-Shiny Erstellung",
                            ),
                   tabPanel("Sonstiges",
                                     verbatimTextOutput("about")
                            )
                 )
        )

           
           
                 