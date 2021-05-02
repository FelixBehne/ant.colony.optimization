
function(input, output, session) {
  
  output$textOne <- renderText({ "Darstellung der Optimierungsfunktion" })
  output$plotOne <- renderPlot({
    returnPlot("rosenbrock", input$intervalMinR, input$intervalMaxR, input$thetaR,input$phiR, input$shadeR, "green")
  })
  
  output$textTwo <- renderText({ "Himmelblau Funktion" })
  output$plotTwo <- renderPlot({
    returnPlot("himmelblau", input$intervalMinH, input$intervalMaxH, input$thetaH, input$phiH, input$shadeH, "orange")
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

  
  
    
    


  
