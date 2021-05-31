#' algorithm UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @import shiny shinycssloaders htmltools
#'
#' @noRd
mod_algorithm_tab_ui <- function(id) {
  ns <- shiny::NS(id) # nolint
  shiny::tagList(
    shiny::titlePanel("Übertragung auf Algorithmen"),
    htmltools::br(),
    htmltools::h4("1. Berechne die bedingte Wahrscheinlichkeit, dass eine Ameise sich für einen bestimmten Weg entscheidet,
                        ausgehend von ihrem aktuellen Standort"),
    shiny::withMathJax(),
    htmltools::tags$head(
      htmltools::tags$style(
        htmltools::HTML(".MathJax {font-size: 4em !important;}")
      )
    ),
    shiny::fluidRow(
      shiny::column(
        10,
        shinycssloaders::withSpinner(uiOutput(ns("formel_one")))
      ),
      shiny::column(
        2,
        bs4Dash::actionButton("infobuttonFormel1", label = "", width = "60px", icon = icon("info"))
      )
    ),
    htmltools::h4("2. Berechne den neuen Pheromonwert nach partieller Verdunstung der alten Pheromone und Verteilung der neuen Pheromone"),
    shiny::fluidRow(
      shiny::column(
        10,
        shinycssloaders::withSpinner(uiOutput(ns("formel_two")))
      ),
      shiny::column(
        2,
        bs4Dash::actionButton("infobuttonFormel2", label = "", width = "60px", icon = icon("info"))
      )
    ),
    htmltools::h4("3. Belohnung mit Pheromonwerten"),
    shiny::fluidRow(
      shiny::column(
        10,
        shinycssloaders::withSpinner(uiOutput((ns("formel_three"))))
      ),
      shiny::column(
        2,
        bs4Dash::actionButton("infobuttonFormel3", label = "", width = "60px", icon = icon("info"))
      )
    )
  )
}

#' algorithm Server Functions
#'
#' @import shiny shinyWidgets
#'
#' @noRd
mod_algorithm_tab_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns # nolint
    # output element for formula of the probability of an ant to decide to pass a specific path-section
    output$formel_one <- shiny::renderUI({
      shiny::withMathJax(
        shiny::helpText("
              $$P(c_r|s_a[c_l]) = \\begin{cases} \\frac{\\eta_r^\\alpha \\cdot \\tau_r^\\beta}{\\sum\\limits_{c_u\\in J(s_a[c_l])}
              \\eta_u^\\alpha \\cdot \\tau_u^\\beta} & \\text{wenn $c_r\\in J(s_\\alpha [c_l])$,} \\\\ 0 & \\text{sonst.} \\end{cases}\\!$$
               ")
      )
    })
    # output element for formula to calculate the new pheromone level 
    output$formel_two <- shiny::renderUI({
      shiny::withMathJax(
        shiny::helpText("$$\\tau_j=(1-\\rho)\\cdot\\tau_j + \\sum\\limits_{\\alpha\\in A} \\Delta \\tau_j^{s_a}\\!$$
               ")
      )
    })
    # output element for formula to calculate the additional amount of pheromone
    output$formel_three <- shiny::renderUI({
      shiny::withMathJax(
        shiny::helpText("$$\\Delta \\tau_j^{s_a} = \\begin{cases} F(s_a), & \\text{wenn $c_j$ eine Komponente von $s_a$ ist} \\\\
               0 & \\text{sonst.} \\end{cases}\\!$$
               mit Wahrscheinlichkeit \\(P\\), Ameise \\(s_a\\), aktueller Pfad-Abschnitt \\(c_l\\), potenzieller nächster Pfad-Abschnitt
               \\(c_r\\), Menge der wählbaren Pfad-Abschnitte \\(J\\), Pheromonwert des Pfad-Abschnitts \\(\\tau\\), konstantem
               Verdunstungsfaktor \\(\\rho\\)")
      )
    })
    # Event-Listener for the infobutton for the first formula 
    shiny::observeEvent(input$infobuttonFormel1, {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Prinzip",
        text = "Wir berechnen für jeden Pfadabschnitt die Wahrscheinlichkeit, dass eine Ameise sich als nächstes für diesen Pfadabschnitt entscheidet.
      Wenn der Pfad-Abschnitt von der derzeitigen Position der Ameise aus nicht erreichbar ist, ist die Wahrscheinlichkeit Null.
      Ansonsten ist die Wahrscheinlichkeit von der Pheromonkonzentration des Pfad-Abschnitts und von problemspezifischen Parametern wie beispielsweise
      der Pfadlänge abhängig.",
        type = "info"
      )
    })
    # Event-Listener for the Infobutton for the second formula 
    shiny::observeEvent(input$infobuttonFormel2, {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Aktualisierung der Pheromonkonzentration",
        text = "Das Pheromonlevel wird aktualisiert: Die bisherige Pheromonkonzentration der Pfad-Abschnitte wird um einen bestimmten,
      konstanten Wert verringert (Verdunstung) und die Pfad-Abschnitte, die von Ameisen gewählt und begangen worden sind, bekommen neues Pheromon",
        type = "info"
      )
    })
    # Event-Listener for the Infobutton for the third formula 
    shiny::observeEvent(input$infobuttonFormel3, {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Belohnung mit Pheromon",
        text = "Wir betrachten die einzelnen Pfad-Abschnitte nacheinander: Die Höhe ihrer Pheromon-\"Belohnung\" wird mittels einer
      problemspezifischen Funktion ermittelt, wenn der Pfad-Abschnitt von der Ameise begangen wurde. Abschnitte, die nicht von der Ameise
      begangen wurden, bekommen kein Pheromon.",
        type = "info"
      )
    })
  })
}
