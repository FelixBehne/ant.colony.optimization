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
#' @param input_g INput from the global server that contains the controlbar inputs.
#'
#' @import shiny
#'
#' @noRd
mod_rosenbrock_tab_server <- function(id, input_g) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns # nolintr
    # output element for rendering the 3d plot of the Rosenbrock function
    output$rosenbrock <- shiny::renderPlot({
      return_3d_plot(
        fu = "rosenbrock",
        minim = input_g$lower_bound_test,
        maxim = input_g$upper_bound_test,
        theta_input = input_g$theta,
        phi_input = input_g$phi,
        shade_input = input_g$shade,
        colour = "green"
      )
    })
    # output element to show actual minimum of the Rosenbrock function
    output$result_actual <- shiny::renderTable(minima_rosenbrock)


    # output element to show the minimum calculated by ACO (with package evoper)
    output$result_aco <- shiny::renderTable({
      print(input_g$iterations_test)
      print(input_g$lower_bound_test)
      print(input_g$upper_bound_test)
      calculate_min(
        iterations = input_g$iterations_test,
        lower_bound = input_g$lower_bound_test,
        upper_bound = input_g$upper_bound_test,
        test_function = "rosenbrock"
      )
    })
  })
}
