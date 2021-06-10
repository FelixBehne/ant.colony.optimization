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
        shiny::fluidRow(
          shiny::column(
            10,
            shinycssloaders::withSpinner(shiny::tableOutput(ns(
              "result_aco"
            )))
          ),
          shiny::column(
            2,
            bs4Dash::actionButton(
              ns("show_dif_to_real_minimum_button"),
              label = "",
              width = "60px",
              icon = icon("info")
            )
          )
        )
      ),
      bs4Dash::box(
        id = "rosenbrock",
        title = "Result Actual",
        maximizable = TRUE,
        collapsible = TRUE,
        closable = TRUE,
        width = 6,
        shiny::fluidRow(
          shiny::column(
            10,
            shinycssloaders::withSpinner(shiny::tableOutput(
              ns("result_actual")
            ))
          ),
          shiny::column(
            2,
            bs4Dash::actionButton(
              ns("rosenbrock_formula_button"),
              label = "",
              width = "60px",
              icon = icon("info")
            )
          )
        )
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
      calculate_min(
        iterations = input_g$iterations_test,
        lower_bound = input_g$lower_bound_test,
        upper_bound = input_g$upper_bound_test,
        test_function = "rosenbrock"
      )
    })
    # output element to show the difference of the minimum calculated by ACO (with package evoper) and the actual minimum in a table
    output$minima_diff_table <- shiny::renderTable({
      min_aco_df <- calculate_min(
        iterations = input_g$iterations_test,
        lower_bound = input_g$lower_bound_test,
        upper_bound = input_g$upper_bound_test,
        test_function = "rosenbrock"
      )
      calc_abs_diff_to_actual_min(min_aco_df, minima_rosenbrock)
    })

    # Event-Listener for the Infobutton showing the difference of the calculated and the real minimum
    shiny::observeEvent(input$show_dif_to_real_minimum_button, {
      shinyalert::shinyalert(
        title = "Absolute difference to the actual minimum",
        text = tagList(shinycssloaders::withSpinner(uiOutput(
          ns("ui_table_dif")
        ))),
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
    # align the table with the difference of the calculated and the actual minimum centrally
    output$ui_table_dif <- renderUI({
      fluidRow(column(
        12,
        align = "center",
        shinycssloaders::withSpinner(shiny::tableOutput(ns(
          "minima_diff_table"
        )))
      ))
    })

    # Event-Listener for the Infobutton for the Rosenbrock formula
    shiny::observeEvent(input$rosenbrock_formula_button, {
      shinyalert::shinyalert(
        title = "Formula of the Rosenbrock Function",
        text = tagList(shinycssloaders::withSpinner(uiOutput(
          ns("rose_formula")
        ))),
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
    # Render the formula of the Rosenbrock function for the Info Button with central alignment
    output$rose_formula <- renderUI({
      fluidRow(column(12,
        align = "center",
        withMathJax(
          helpText(
            "$$z(x,y)=(a-x)^2+b(y-x^2)^2$$
                          with \\(a\\)=1, \\(b\\)=100"
          )
        )
      ))
    })
  })
}
