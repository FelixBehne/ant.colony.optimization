
function(input, output, session) {
  
  output$textOne <- renderText({ "Darstellung der Optimierungsfunktion" })
 
  output$plotOne <- renderPlot({

    rosenbrock <- function(x, y){
      (1-x)^2+100*(y-x^2)^2
    }
    x <- y <- seq(input$intervalMinR, input$intervalMaxR, length= 20)
    z <- outer(x, y, rosenbrock )
    persp(x, y, z, 
          main = "Rosenbrock-Funktion mit a=1, b=100)",
          theta = input$thetaR, phi = input$phiR,
          shade = input$shadeR, col = "green")
  })
  
  output$textTwo <- renderText({ "Himmelblau Funktion" })
  
  output$plotTwo <- renderPlot({
    himmelblau <- function(x, y){
      (x^2+y-11)^2+(x+y^2-7)^2
    }
      x <- y <- seq(input$intervalMinH, input$intervalMaxH, length= 20)
      z <- outer(x, y, himmelblau )
      
      persp(x, y, z,
            main="Himmelblau-Funktion",
            theta = input$thetaH, phi = input$phiH,
            col = "orange", shade = 0.5)
    
  })

 # output$textAntAlg = DT::renderDataTable({
    # Create the data frame.
 #   MinimaRosenbrock
    
 # })
  output$minRoseText <- renderText({"tatsächliches Minimum der Rosenbrockfunktion: "})
  output$tableMinimaRose <- renderTable(MinimaRosenbrock)
  output$textAntRose <- renderText({"Ergebnis des Algorithmus:"})
  output$tableAntRose <- renderTable({calculateMin(input$iterationsR,input$intervalMinR,input$intervalMaxR,'rosenbrock')})

   
  
  output$minHimText <- renderText({"Minima der Himmelblaufunktion: "})
  output$tableMinimaHim <- renderTable(MinimaHimmelblau)
  output$textAntHim <- renderText({"Ergebnis des Algorithmus:"})
  output$tableAntHim <- renderTable({calculateMin(input$iterationsH,input$intervalMinH,input$intervalMaxH,'himmelblau')})
  
  

  
  
}

  
  
    
    


  
