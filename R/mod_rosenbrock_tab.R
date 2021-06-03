#' rosenbrock_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @import shiny shinycssloaders bs4Dash
#'
#' @noRd
mod_rosenbrock_tab_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::titlePanel("Rosenbrock Function"),
    bs4Dash::box(
      id = "rosenbrock",
      title = "Rosenbrock-Function with a=1, b=100",
      maximizable = TRUE,
      collapsible = TRUE,
      closable = TRUE,
      width = 12,
      shinycssloaders::withSpinner(shiny::plotOutput(ns("rosenbrock")))
    ),
    shiny::fluidRow(
      bs4Dash::box(
        id = "rosenbrock",
        title = "Result ACO",
        maximizable = TRUE,
        collapsible = TRUE,
        closable = TRUE,
        width = 6,
        shinycssloaders::withSpinner(shiny::tableOutput(ns("result_aco")))
      ),
      bs4Dash::box(
        id = "rosenbrock",
        title = "Result Actual",
        maximizable = TRUE,
        collapsible = TRUE,
        closable = TRUE,
        width = 6,
        shinycssloaders::withSpinner(shiny::tableOutput(ns("result_actual")))
      ),
    )
  )
}

#' rosenbrock_tab Server Functions
#'
#' @param id Ui Module Id. Needed to allocate the right inputs to the right outputs.
#' @param input_c INput from the global server that contains the controlbar inputs.
#'
#' @import shiny
#'
#' @noRd
mod_rosenbrock_tab_server <- function(id, input_c) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns # nolintr
    # output element for rendering the 3d plot of the Rosenbrock function
    output$rosenbrock <- shiny::renderPlot({
      return_3d_plot(
        fu = "rosenbrock",
        minim = input_c$interval_min,
        maxim = input_c$interval_max,
        theta_input = input_c$theta,
        phi_input = input_c$phi,
        shade_input = input_c$shade,
        colour = "green"
      )
    })
    # output element to show actual minimum of the Rosenbrock function
    output$result_actual <- shiny::renderTable(minima_rosenbrock)


    # output element to show the minimum calculated by ACO (with package evoper)
    output$result_aco <- shiny::renderTable({
      calculate_min(
        iter = input_c$iterations,
        minim = input_c$interval_min,
        maxim = input_c$interval_max,
        fu = "rosenbrock"
      )
    })
  })
}
