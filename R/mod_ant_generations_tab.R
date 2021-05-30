#' ant_generations_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ant_generations_tab_ui <- function(id) {
  ns <- NS(id)
  tagList(
    titlePanel("Ant Generations Plot"),
    bs4Dash::box(
      id = "generations",
      title = "Generations of ants in search of the minimum of the Himmelblaufunction",
      maximizable = TRUE,
      collapsible = TRUE,
      closable = TRUE,
      width = 12,
      height = "100%",
      shinycssloaders::withSpinner(plotly::plotlyOutput(ns("generation"))),
      br()
    ),
    #  shiny::fluidRow(
    bs4Dash::box(
      id = "generations",
      title = "Minima Himmelblau",
      maximizable = TRUE,
      collapsible = TRUE,
      closable = TRUE,
      width = 12,
      height = "100%",
      shinycssloaders::withSpinner(shiny::tableOutput(ns("table_minima")))
    ),
    # bs4Dash::box(
    #   id = "generations",
    #   title = "Number Generations",
    #   maximizable = TRUE,
    #   collapsible = TRUE,
    #   closable = TRUE,
    #   width = 12,
    #   height = "100%",
    #   titlePanel("Test")
    #   # textOutput("gen_numb")
    # ))
  )
}



#' ant_generations_tab Server Functions
#'
#' @noRd
mod_ant_generations_tab_server <- function(id, input_c) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Compute Plot requirements
    vars <- shiny::eventReactive(
      eventExpr = input_c$start_gen,
      valueExpr = {
        data.frame(x1 = c(input_c$lower_bound, input_c$upper_bound), x2 = c(input_c$lower_bound, input_c$upper_bound))
      },
      ignoreNULL = FALSE
    )

    # Plot based on computed data
    output$generation <- plotly::renderPlotly({
      generation1 <- make_start_set(number_ants = input_c$hor_numb, start_interval = vars())
      xyf <- calc_gens(
        cost_f = cost_function_himmelblau,
        param_list = vars(),
        gen_p = generation1,
        gen = input_c$gen_numb
      )
      plot_data <- prepare_for_plot(
        hor_number = input_c$hor_numb,
        xyf = xyf
      )
      plotly::plot_ly(x = plot_data$x, y = plot_data$y, z = plot_data$f, type = "scatter3d", mode = "markers", color = plot_data$colour)
    })
    # output$generationNumber <- renderText({
    #   toString(input_c$generationenAnzahl)
    # })
    output$table_minima <- renderTable(minima_himmelblau2)
  })
}
