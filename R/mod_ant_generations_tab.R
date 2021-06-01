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
#' @import shiny plotly
#'
#' @noRd
mod_ant_generations_tab_server <- function(id, input_c) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # set the range of the x, y and f value in which we search the minimum and update it on user inputs
    vars <- shiny::eventReactive(
      eventExpr = input_c$start_gen,
      valueExpr = {
        data.frame(x1 = c(input_c$lower_bound, input_c$upper_bound), x2 = c(input_c$lower_bound, input_c$upper_bound))
      },
      ignoreNULL = FALSE
    )

    # Plot the location of the ants, their mean value and the actual minima of the objective function in a scatter plot
    output$generation <- plotly::renderPlotly({
      # create the locations of the first generation randomly 
      generation1 <- make_start_set(
        number_ants = input_c$hor_numb,
        start_interval = vars()
      )
      # calculate the new locations of the ants after a specified number of iterations
      xyf <- calc_gens(
        cost_f = cost_function_himmelblau,
        param_list = vars(),
        gen_p = generation1,
        gen = input_c$gen_numb
      )
      # add the mean value of the ants' location, furthermore add the locations of the actual minima and add
      # a column for the colour of the points in the plot 
      plot_data <- prepare_for_plot(
        hor_number = input_c$hor_numb,
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
    output$table_minima <- shiny::renderTable(minima_himmelblau2)
    
  })
}
