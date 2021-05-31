#' tsp_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tsp_tab_ui <- function(id) {
  ns <- NS(id)
  tagList(
    titlePanel("Travelling Salesman Problem"),
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
          shinycssloaders::withSpinner(shiny::dataTableOutput(ns("table")))
        ),
       
       bs4Dash::box(
        id = "UseCases",
        title = "Anwendungen",
        maximizable = TRUE,
        collapsible = TRUE,
        closable = TRUE,
        width = 12,
        height = "100%",
        includeMarkdown(app_sys("app/www/rmd/use_cases.Rmd"))
      )
    )
    # fluidRow(
    #   style = "background-color:	#E8E8E8;",
    #   column(
    #     10,
    #     includeMarkdown("Anwendung.Rmd")
    #   )
    # )
  )
}

#' tsp_tab Server Functions
#'
#' @noRd
mod_tsp_tab_server <- function(id, input_c) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Returns the plot with the cities
    output$tsp_plot <- shiny::renderPlot({
      x <- getXValues()
      y <- getYValues()
      labels <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
      plot(x, y, col = 2, pch = 16, xlab = "", ylab = "")
      text(x, y, labels = labels, cex = 1.2, pos = 2)
    })

    # Here the function acoAlg starts and be fuled with the values from the sliders
    ntext <- eventReactive(input_c$action, {
      options <- aco_tsp(
        x= getXValues(),
        y = getYValues(),
        alpha = input_c$alpha,
        beta = input_c$beta,
        evaporation = input_c$evaporation,
        randomness_factor = input_c$randomness_f,
        nOfAnts = input_c$numb_ants,
        iterations = input_c$iterations
      )
    })
    # Returns the Table but waits until the actionButton is activated.
    output$table <- shiny::renderDataTable({
      options <- ntext()
    })
    shiny::observeEvent(input_c$info, {
      shiny::showModal(shiny::modalDialog(
        title = "Info",
        includeMarkdown(app_sys("app/www/rmd/tsp.Rmd"))
      ))
    })
  })
}
