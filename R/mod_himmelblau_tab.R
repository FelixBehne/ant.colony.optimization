#' himmelblau_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @import shiny bs4Dash shinycssloaders
#'
#' @noRd
mod_himmelblau_tab_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::titlePanel("Himmelblau Function"),
    bs4Dash::box(
      id = "himmelblau",
      title = "Himmelblau-Function",
      maximizable = TRUE,
      collapsible = TRUE,
      closable = TRUE,
      width = 12,
      height = "100%",
      shinycssloaders::withSpinner(shiny::plotOutput(ns("himmelblau")))
    ),
    shiny::fluidRow(
      bs4Dash::box(
        id = "result_aco",
        title = "Result ACO",
        maximizable = TRUE,
        collapsible = TRUE,
        closable = TRUE,
        width = 6,
        shinycssloaders::withSpinner(shiny::tableOutput(ns("result_aco")))
      ),
      bs4Dash::box(
        id = "result_actual",
        title = "Result Actual",
        maximizable = TRUE,
        collapsible = TRUE,
        closable = TRUE,
        width = 6,
        shinycssloaders::withSpinner(shiny::tableOutput(ns("result_actual")))
      )
    )
  )
}

#' himmelblau_tab Server Functions
#'
#' @param id Ui Module Id. Needed to allocate the right inputs to the right outputs.
#' @param input_c INput from the global server that contains the controlbar inputs.
#'
#' @import shiny
#'
#' @noRd
mod_himmelblau_tab_server <- function(id, input_c) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns # nolintr

    output$himmelblau <- shiny::renderPlot({
      return_3d_plot(
        fu = "himmelblau",
        minim = input_c$interval_min,
        maxim = input_c$interval_max,
        theta_input = input_c$theta,
        phi_input = input_c$phi,
        shade_input = input_c$shade,
        colour = "green"
      )
    })

    output$result_actual <- shiny::renderTable(minima_himmelblau)

    output$result_aco <- shiny::renderTable({
      calculate_min(
        iter = input_c$iterations,
        minim = input_c$interval_min,
        maxim = input_c$interval_max,
        fu = "himmelblau"
      )
    })
  })
}
