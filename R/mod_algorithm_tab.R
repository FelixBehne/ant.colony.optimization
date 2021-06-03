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
    shiny::titlePanel("Algorithm Definition"),
    br(),
    bs4Dash::box(
      id = "formula_one_box",
      title = "Calculate the conditional probability that an ant, given its current location, will choose a particular path.",
      maximizable = TRUE,
      collapsible = TRUE,
      closable = TRUE,
      width = 12,
      shiny::withMathJax(),
      htmltools::tags$head(
        htmltools::tags$style(
          htmltools::HTML(".MathJax {font-size: 4em !important;}")
        )
      ),
      shiny::fluidRow(
        shiny::column(
          10,
          shinycssloaders::withSpinner(uiOutput(ns("formula_one")))
        ),
        shiny::column(
          2,
          bs4Dash::actionButton(ns("info_formula_one"), label = "", width = "60px", icon = icon("info"))
        )
      )
    ),
    br(),
    bs4Dash::box(
      id = "formula_two_box",
      title = "Calculate the new pheromone value after partial evaporation of the old pheromones and distribution of the new pheromones",
      maximizable = TRUE,
      collapsible = TRUE,
      closable = TRUE,
      width = 12,
      shiny::withMathJax(),
      htmltools::tags$head(
        htmltools::tags$style(
          htmltools::HTML(".MathJax {font-size: 4em !important;}")
        )
      ),
      shiny::fluidRow(
        shiny::column(
          10,
          shinycssloaders::withSpinner(uiOutput(ns("formula_two")))
        ),
        shiny::column(
          2,
          bs4Dash::actionButton(ns("info_formula_two"), label = "", width = "60px", icon = icon("info"))
        )
      )
    ),
    br(),
    bs4Dash::box(
      id = "formula_three_box",
      title = "Reward with pheromones",
      maximizable = TRUE,
      collapsible = TRUE,
      closable = TRUE,
      width = 12,
      shiny::withMathJax(),
      htmltools::tags$head(
        htmltools::tags$style(
          htmltools::HTML(".MathJax {font-size: 4em !important;}")
        )
      ),
      shiny::fluidRow(
        shiny::column(
          10,
          shinycssloaders::withSpinner(uiOutput(ns("formula_three")))
        ),
        shiny::column(
          2,
          bs4Dash::actionButton(ns("info_formula_three"), label = "", width = "60px", icon = icon("info"))
        )
      )
    ),
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
    output$formula_one <- renderUI({
      withMathJax(
        helpText("
              $$P(c_r|s_a[c_l]) = \\begin{cases} \\frac{\\eta_r^\\alpha \\cdot \\tau_r^\\beta}{\\sum\\limits_{c_u\\in J(s_a[c_l])}
              \\eta_u^\\alpha \\cdot \\tau_u^\\beta} & \\text{if $c_r\\in J(s_\\alpha [c_l])$,} \\\\ 0 & \\text{otherwise.} \\end{cases}\\!$$
               ")
      )
    })
    # output element for formula to calculate the new pheromone level
    output$formula_two <- shiny::renderUI({
      shiny::withMathJax(
        shiny::helpText("$$\\tau_j=(1-\\rho)\\cdot\\tau_j + \\sum\\limits_{\\alpha\\in A} \\Delta \\tau_j^{s_a}\\!$$
               ")
      )
    })
    # output element for formula to calculate the additional amount of pheromone
    output$formula_three <- shiny::renderUI({
      shiny::withMathJax(
        shiny::helpText(
          "$$\\Delta \\tau_j^{s_a} = \\begin{cases} F(s_a), & \\text{if $c_j$ is a component of $s_a$ } \\\\
               0 & \\text{otherwise.} \\end{cases}\\!$$
               with probability \\(P\\), ant \\(s_a\\), current path intercept \\(c_l\\), potential next path intercept
               \\(c_r\\), set of selectable pathway segments \\(J\\), pheromone value of pathway segment \\(\\tau\\), constant
               evaporation factor \\(\\rho\\)"
        )
      )
    })

    # Event-Listener for the infobutton for the first formula
    shiny::observeEvent(input$info_formula_one, {
      shinyalert::shinyalert(
        title = "Fundamental principle",
        text = "For each path section, we calculate the probability that an ant will choose that path section next.
                  If the path section is not accessible from the ant's current position, the probability is zero.
                  Otherwise, the probability depends on the pheromone concentration of the path segment and on problem-specific
                  parameters such as the path length.",
        size = "m",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "info",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#249c24",
        animation = TRUE
      )
    })

    # Event-Listener for the infobutton for the second formula
    shiny::observeEvent(input$info_formula_two, {
      shinyalert::shinyalert(
        title = "Updating the pheromone concentration",
        text = "The pheromone level is updated: The previous pheromone concentration of the path sections
                  is reduced by a certain constant value (evaporation) and the path sections that have been
                  chosen and walked on by ants are given new pheromone.",
        size = "m",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "info",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#249c24",
        animation = TRUE
      )
    })

    # Event-Listener for the Infobutton for the third formula
    shiny::observeEvent(input$info_formula_three, {
      shinyalert::shinyalert(
        title = "Pheromone Reward",
        text = "We consider the individual path sections one after the other:
                  The amount of their pheromone \"reward\" is determined by means of a problem-specific function.
                  Sections that have not been entered by the ant do not get any pheromone.",
        size = "m",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "info",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#249c24",
        animation = TRUE
      )
    })
  })
}
