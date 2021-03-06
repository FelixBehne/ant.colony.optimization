#' tsp_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @import shiny bs4Dash shinycssloaders
#'
#' @noRd
mod_tsp_tab_ui <- function(id) {
  ns <- shiny::NS(id) # nolint
  shiny::tagList(
    shiny::titlePanel("Traveling Salesman Problem"),
    shiny::fluidRow(
      bs4Dash::box(
        id = "tsp_plot",
        title = "Traveling Salesman Problem",
        maximizable = TRUE,
        collapsible = TRUE,
        closable = TRUE,
        width = 12,
        height = "100%",
        shinycssloaders::withSpinner(shiny::plotOutput(ns("tsp_plot")))
      ),
      bs4Dash::box(
        id = "tsp_plot",
        title = "Traveling Salesman Table",
        maximizable = TRUE,
        collapsible = TRUE,
        closable = TRUE,
        width = 12,
        height = "100%",
        shinycssloaders::withSpinner(shiny::tableOutput(ns("table")))
      ),
      bs4Dash::box(
        id = "UseCases",
        title = "Use Cases",
        maximizable = TRUE,
        collapsible = TRUE,
        closable = TRUE,
        width = 12,
        height = "100%",
        htmltools::includeMarkdown(app_sys("app/www/rmd/use_cases.Rmd"))
      )
    )
  )
}

#' tsp_tab Server Functions
#'
#' @import graphics shiny
#'
#' @noRd
mod_tsp_tab_server <- function(id, input_g) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Returns the plot with the cities
    output$tsp_plot <- shiny::renderPlot({
      x <- get_x_values()
      y <- get_y_values()
      labels <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
      graphics::plot(x, y, col = 2, pch = 16, xlab = "", ylab = "")
      graphics::text(x, y, labels = labels, cex = 1.2, pos = 2)
    })

    # Here the function acoAlg starts and be fuled with the values from the sliders
    ntext <- shiny::eventReactive(input_g$action, {
      options <- aco_tsp(
        x = get_x_values(),
        y = get_y_values(),
        alpha = input_g$alpha,
        beta = input_g$beta,
        evaporation = input_g$evaporation,
        randomness_factor = input_g$randomness_f,
        numb_ants = input_g$numb_ants,
        iterations = input_g$iterations
      )
    })
    # Returns the Table but waits until the actionButton is activated.
    output$table <- shiny::renderTable(
      ntext(),
      width = "100%"
    )
    shiny::observeEvent(input_g$info, {
      shiny::showModal(shiny::modalDialog(
        title = "Info",
        includeMarkdown(app_sys("app/www/rmd/tsp.Rmd"))
      ))
    })
    shiny::observeEvent(
      eventExpr = input_g$alpha_info,
      handlerExpr = {
        shinyalert::shinyalert(
          title = "Alpha",
          text = "The strength of the pheromone. The higher the stronger.",
          size = "s",
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
      }
    )
    shiny::observeEvent(
      eventExpr = input_g$beta_info,
      handlerExpr = {
        shinyalert::shinyalert(
          title = "Beta",
          text = "The Relevance of the distance. The higher the more decreases the relevance.",
          size = "s",
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
      }
    )
    shiny::observeEvent(
      eventExpr = input_g$evaporation_info,
      handlerExpr = {
        shinyalert::shinyalert(
          title = "Evaporation",
          text = "Strength with which the pheromones evaporate.",
          size = "s",
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
      }
    )
    shiny::observeEvent(
      eventExpr = input_g$randomness_f_info,
      handlerExpr = {
        shinyalert::shinyalert(
          title = "Randomness Factor",
          text = "Factor for random iterations.",
          size = "s",
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
      }
    )
    shiny::observeEvent(
      eventExpr = input_g$numb_ants_info,
      handlerExpr = {
        shinyalert::shinyalert(
          title = "Number of Ants",
          text = "The Number of ants to use for the calculation",
          size = "s",
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
      }
    )
    shiny::observeEvent(
      eventExpr = input_g$iterations_info,
      handlerExpr = {
        shinyalert::shinyalert(
          title = "Iterations",
          text = "The Number of iterations to use for the calculation",
          size = "s",
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
      }
    )
  })
}
