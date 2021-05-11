
function(input, output, session) {

#--Formeln----------------------------
  output$formel_one <- renderUI({
    withMathJax(
      helpText('
              $$P(c_r|s_a[c_l]) = \\begin{cases} \\frac{\\eta_r^\\alpha \\cdot \\tau_r^\\beta}{\\sum\\limits_{c_u\\in J(s_a[c_l])} \\eta_u^\\alpha \\cdot \\tau_u^\\beta} & \\text{wenn $c_r\\in J(s_\\alpha [c_l])$,} \\\\ 0 & \\text{sonst.} \\end{cases}\\!$$
               mit Wahrscheinlichkeit \\(P\\), Ameise \\(s_a\\), aktueller Pfad-Abschnitt \\(c_l\\), potenzieller nächster Pfad-Abschnitt \\(c_r\\), Menge der wählbaren Pfad-Abschnitte \\(J\\), Pheromonwert des Pfad-Abschnitts \\(\\tau\\)'))
  })
  
  output$formel_two <- renderUI({
    withMathJax(
      helpText('$$\\tau_j=(1-\\rho)\\cdot\\tau_j + \\sum\\limits_{\\alpha\\in A} \\Delta \\tau_j^{s_a}\\!$$
               mit konstantem Verdunstungsfaktor \\(\\rho\\)'))
  })
  
  output$formel_three <- renderUI({
    withMathJax(
      helpText('$$\\Delta \\tau_j^{s_a} = \\begin{cases} F(s_a), & \\text{wenn $c_j$ eine Komponente von $s_a$ ist} \\\\
               0 & \\text{sonst.} \\end{cases}\\!$$'))
  })
#----------------------------------
  
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








