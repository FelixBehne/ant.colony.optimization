
function(input, output, session) {

#--Formeln----------------------------
  output$formel_one <- renderUI({
    withMathJax(
      helpText('
              $$P(c_r|s_a[c_l]) = \\begin{cases} \\frac{\\eta_r^\\alpha \\cdot \\tau_r^\\beta}{\\sum\\limits_{c_u\\in J(s_a[c_l])} 
              \\eta_u^\\alpha \\cdot \\tau_u^\\beta} & \\text{wenn $c_r\\in J(s_\\alpha [c_l])$,} \\\\ 0 & \\text{sonst.} \\end{cases}\\!$$
               '))
  })
  output$formel_two <- renderUI({
    withMathJax(
      helpText('$$\\tau_j=(1-\\rho)\\cdot\\tau_j + \\sum\\limits_{\\alpha\\in A} \\Delta \\tau_j^{s_a}\\!$$
               '))
  })
  output$formel_three <- renderUI({
    withMathJax(
      helpText('$$\\Delta \\tau_j^{s_a} = \\begin{cases} F(s_a), & \\text{wenn $c_j$ eine Komponente von $s_a$ ist} \\\\
               0 & \\text{sonst.} \\end{cases}\\!$$
               mit Wahrscheinlichkeit \\(P\\), Ameise \\(s_a\\), aktueller Pfad-Abschnitt \\(c_l\\), potenzieller nächster Pfad-Abschnitt 
               \\(c_r\\), Menge der wählbaren Pfad-Abschnitte \\(J\\), Pheromonwert des Pfad-Abschnitts \\(\\tau\\), konstantem 
               Verdunstungsfaktor \\(\\rho\\)'))
  })
#--ActionButtons für Erklärung zu Formeln ---------------------
  
  observeEvent(input$infobuttonFormel1, {
    shinyWidgets::sendSweetAlert(
      session = session, 
      title = "Prinzip",
      text = "Wir berechnen für jeden Pfadabschnitt die Wahrscheinlichkeit, dass eine Ameise sich als nächstes für diesen Pfadabschnitt entscheidet. 
      Wenn der Pfad-Abschnitt von der derzeitigen Position der Ameise aus nicht erreichbar ist, ist die Wahrscheinlichkeit Null. Ansonsten ist die 
      Wahrscheinlichkeit von der Pheromonkonzentration des Pfad-Abschnitts und von problemspezifischen Parametern wie beispielsweise der Pfadlänge abhängig.",
      type = "info"
    )
  })
  observeEvent(input$infobuttonFormel2, {
    shinyWidgets::sendSweetAlert(
      session = session, 
      title = "Aktualisierung der Pheromonkonzentration",
      text = "Das Pheromonlevel wird aktualisiert: Die bisherige Pheromonkonzentration der Pfad-Abschnitte wird um einen bestimmten, 
      konstanten Wert verringert (Verdunstung) und die Pfad-Abschnitte, die von Ameisen gewählt und begangen worden sind, bekommen neues Pheromon",
      type = "info"
    )
  })
  observeEvent(input$infobuttonFormel3, {
    shinyWidgets::sendSweetAlert(
      session = session, 
      title = "Belohnung mit Pheromon",
      text = "Wir betrachten die einzelnen Pfad-Abschnitte nacheinander: Die Höhe ihrer Pheromon-\"Belohnung\" wird mittels einer 
      problemspezifischen Funktion ermittelt, wenn der Pfad-Abschnitt von der Ameise begangen wurde. Abschnitte, die nicht von der Ameise 
      begangen wurden, bekommen kein Pheromon.",
      type = "info"
    )
  })

#--------------- Visualisierung des Algorithmus-----------------------------------------
  
# --------------Anwendung auf Rosenbrockfunktion---------------
  
  output$plotOne <- renderPlot({
    returnPlot("rosenbrock", input$intervalMinR, input$intervalMaxR, input$thetaR,input$phiR, input$shadeR, "green")
  })
  
  output$minRoseText <- renderText({"tatsächliches Minimum der Rosenbrockfunktion: "})
  output$tableMinimaRose <- renderTable(MinimaRosenbrock)
  output$textAntRose <- renderText({"Ergebnis des Algorithmus:"})
  output$tableAntRose <- renderTable({calculateMin(input$iterationsR,input$intervalMinR,input$intervalMaxR,'rosenbrock')})
  
  # --------------Anwendung auf Himmelblaufunktion---------------
  
  output$plotTwo <- renderPlot({
    returnPlot("himmelblau", input$intervalMinH, input$intervalMaxH, input$thetaH, input$phiH, input$shadeH, "orange")
  })
  
  output$minHimText <- renderText({"Minima der Himmelblaufunktion: "})
  output$tableMinimaHim <- renderTable(MinimaHimmelblau)
  output$textAntHim <- renderText({"Ergebnis des Algorithmus:"})
  output$tableAntHim <- renderTable({calculateMin(input$iterationsH,input$intervalMinH,input$intervalMaxH,'himmelblau')})
  
#--------------------------Plot Generations of ants on Himmelblau function-----------------------------

  vars <- eventReactive(input$showGen, {
    data.frame(x1=c(input$uGrenze,input$oGrenze),x2=c(input$uGrenze,input$oGrenze))
  })
  
  output$generationNumber <- renderText({
    paste0("Nach " , input$generationenAnzahl, " Generationen")
  })
  output$tableMinimaHim2 <- renderTable(MinimaHimmelblau2)
  
  output$generation <- renderPlotly({
    generation1 = makeStartSet(numberOfAnts=input$horNumb, anfangsintervall=vars())
    ameisenwerte_xyf = ACO_calcGens(costF=costFHImmelblau,paramListR=vars(),genP=generation1,gen=input$generationenAnzahl)
    plotData = prepareForPlot(horNumb=input$horNumb, xyf=ameisenwerte_xyf)
    plot_ly(x=plotData$x, y=plotData$y, z=plotData$f, type="scatter3d", mode="markers", color=plotData$colour)
  })

  # output$meanx <- renderText({ meanX1 })
  # output$meany <- renderText({ meanX2 })
  # output$meanf <- renderText({ meanF })
  
  #--------------------------Anwendungsfälle und TSP----------------------------- 
  
  #returns the plot with the cities
  output$TSPlot <- renderPlot({
    x = getXValues()
    y = getYValues()
    labels = c(1,2,3,4,5,6,7,8,9,10)
    plot(x,y, col =2, pch = 16, xlab ="", ylab ="")
    text(x,y,labels = labels, cex=1.2, pos=2)
  })
  #here the function acoAlg starts and be fuled with the values from the sliders
  
  ntext <- eventReactive(input$action, {
    options = acoAlg(
      getXValues(),
      getYValues(),
      input$alpha,
      input$beta, 
      input$evaporation,
      input$randomnessf,
      input$nOfAnts,
      input$iterations
      )
  })
  #Returns the Table but waits until the actionButton is activated.
  output$table <- renderDataTable({
    options = ntext()
  })
  observeEvent(input$info,{
    showModal(modalDialog(
      title = "Parameter",
      includeMarkdown("Info_TSP.Rmd")
      
    ))
  })
#----------------einf�gen einer Slideshow durch Package slickR----------------------------------------------------
output$slickr <- renderSlickR({
  imgs <- list.files("images/antSlideShow/", pattern=".png", full.names = TRUE)
  slickR(imgs)
})

#---------------Actionbuttons f�r mehr Informationen zu den drei Phasen bei Ameisen-------------------------------
observeEvent(input$infobuttonAF1, {
  shinyWidgets::sendSweetAlert(
    session = session, 
    title = "Phase 1",
    text = "Die Ameisen laufen in unterschiedliche Richtungen um Futter zu suchen. Dabei geben Sie Pheromone (Sexuallockstoffe) ab.",
    type = "info"
  )
})
observeEvent(input$infobuttonAF2, {
  shinyWidgets::sendSweetAlert(
    session = session, 
    title = "Phase 2",
    text = "Die Ameisen orientieren sich an dem Weg mit den meisten Pheromonspuren und nehmen diesen Weg zur Futterbeschaffung.",
    type = "info"
  )
})
observeEvent(input$infobuttonAF3, {
  shinyWidgets::sendSweetAlert(
    session = session, 
    title = "Phase 3",
    text = "Die Ameisen bilden eine sogenannte Ameisenstrasse und behalten diesen Weg auch wenn ein neuer Weg hinzukommt der nicht so lang ist
    wie die Ameisenstrasse.",
    type = "info"
  )
})
}









