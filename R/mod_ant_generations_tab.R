#' ant_generations_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @import shiny bs4Dash shinycssloaders htmltools
#'
#' @noRd
mod_ant_generations_tab_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::titlePanel("Ant Generations Plot"),
    bs4Dash::box(
      id = "generations",
      title = "Generations of ants in search of the minimum of the Himmelblaufunction",
      maximizable = TRUE,
      collapsible = TRUE,
      closable = TRUE,
      width = 12,
      height = "100%",
      shinycssloaders::withSpinner(plotly::plotlyOutput(ns("generation"))),
      htmltools::br()
    ),
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
  )
}



#' ant_generations_tab Server Functions
#'
#' @import shiny plotly thematic
#'
#' @noRd
mod_ant_generations_tab_server <- function(id, input_g) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Plot the location of the ants, their mean value and the actual minima of the objective function in a scatter plot
    output$generation <- plotly::renderPlotly({
      # create the locations of the first generation randomly
      generation1 <- make_start_set(
        number_ants = input_g$hor_numb_ant_generations,
        start_interval = data.frame(x1 = c(input_g$lower_bound_ant_generations, input_g$upper_bound_ant_generations), x2 = c(input_g$lower_bound_ant_generations, input_g$upper_bound_ant_generations)) # nolint
      )
      # calculate the new locations of the ants after a specified number of iterations
      xyf <- calc_gens(
        cost_f = get_test_function(function_name = "himmelblau", numb_parameters = 1),
        param_list = data.frame(x1 = c(input_g$lower_bound_ant_generations, input_g$upper_bound_ant_generations), x2 = c(input_g$lower_bound_ant_generations, input_g$upper_bound_ant_generations)) # nolint
        ,
        gen_p = generation1,
        gen = input_g$gen_numb_ant_generations
      )
      # add the mean value of the ants' location, furthermore add the locations of the actual minima and add
      # a column for the colour of the points in the plot
      plot_data <- prepare_for_plot(
        hor_number = input_g$hor_numb_ant_generations,
        xyf = xyf
      )
      # generate the 3d scatterplot
      plotly::plot_ly(
        x = plot_data$x,
        y = plot_data$y,
        z = plot_data$f,
        type = "scatter3d",
        mode = "markers",
        color = plot_data$colour
      )
    })

    # return a table with the actual minima of the Himmelblau function
    output$table_minima <- shiny::renderTable(minima_himmelblau)
  })
}
