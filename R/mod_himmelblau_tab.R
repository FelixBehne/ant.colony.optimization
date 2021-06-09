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
        shiny::fluidRow(
          shiny::column(
            10,
            shinycssloaders::withSpinner(shiny::tableOutput(ns("result_actual")))
          ),
          shiny::column(
            2,
            bs4Dash::actionButton(ns("himmelblau_button"), label = "", width = "60px", icon = icon("info"))
          )
        )
      ),
    )
  )
}

#' himmelblau_tab Server Functions
#'
#' @param id Ui Module Id. Needed to allocate the right inputs to the right outputs.
#' @param input_g INput from the global server that contains the controlbar inputs.
#'
#' @import shiny
#'
#' @noRd
mod_himmelblau_tab_server <- function(id, input_g) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns # nolintr

    # output element for rendering the 3d plot of the Himmelblau function
    output$himmelblau <- shiny::renderPlot({
      return_3d_plot(
        fu = "himmelblau",
        minim = input_g$lower_bound_test,
        maxim = input_g$upper_bound_test,
        theta_input = input_g$theta,
        phi_input = input_g$phi,
        shade_input = input_g$shade,
        colour = "green"
      )
    })
    # output element to show actual minimum of the Himmelblau function
    output$result_actual <- shiny::renderTable(minima_himmelblau)

    # output element to show the minima calculated by ACO (with package evoper)
    output$result_aco <- shiny::renderTable({
      calculate_min(
        iterations = input_g$iterations_test,
        lower_bound = input_g$lower_bound_test,
        upper_bound = input_g$upper_bound_test,
        test_function = "himmelblau"
      )
    })
    # Event-Listener for the Infobutton for the Himmelblau formula
    shiny::observeEvent(input$himmelblau_button, {
      shinyalert::shinyalert(
        title = "Formula of the Himmelblau Function",
        text = tagList(
          shinycssloaders::withSpinner(uiOutput(ns("him_formula")))
        ),
        size = "m",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = TRUE,
        type = "info",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#249c24",
        animation = TRUE
      )
    })
    # Render the formula of the Himmelblau function for the Info Button with central alignment
    output$him_formula <- renderUI({
      fluidRow(
        column(12,
          align = "center",
          withMathJax(
            helpText("
                  $$z(x,y)=(x^2+y-11)^2+(x+y^2-7)^2$$
                   ")
          )
        )
      )
    })
  })
}
