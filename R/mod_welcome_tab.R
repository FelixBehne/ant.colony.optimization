#' welcome_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @import shiny bs4Dash
#'
#' @noRd
mod_welcome_tab_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bs4Dash::box(
      id = "welcome",
      title = "",
      maximizable = FALSE,
      collapsible = TRUE,
      closable = FALSE,
      width = 12,
      height = "330px",
      shiny::tags$h1("Welcome to the ACO Dashboard !"),
      shiny::tags$h4("We can't wait for you to see our plots"),
      br(),
      br(),
      bs4Dash::actionButton(
        inputId = "show_me",
        label = "Show Me",
        status = "success",
        size = "lg",
        icon = shiny::icon("rocket")
      )
    ),
    br(),
    br(),
    shiny::fluidRow(
      shiny::column(
        width = 6,
        bs4Dash::valueBoxOutput(outputId = ns("app_status"), width = 8),
        br(),
        bs4Dash::valueBoxOutput(outputId = ns("directions"), width = 8)
      ),
      shiny::column(
        width = 6,
        bs4Dash::valueBoxOutput(outputId = ns("app_maintainers"), width = 8),
        br(),
        bs4Dash::valueBoxOutput(outputId = ns("security_and_license"), width = 8),
      )
    )
  )
}

#' welcome_tab Server Functions
#'
#' @noRd
mod_welcome_tab_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$app_status <- renderValueBox({
      valueBox(
        subtitle = "",
        value = "Build Successfull",
        icon = shiny::icon("check-circle"),
        footer = shiny::tags$p("More Info", shiny::icon("arrow-alt-circle-right")),
        color = "olive",
        gradient = TRUE
      )
    })
    output$directions <- renderValueBox({
      valueBox(
        subtitle = "",
        value = "Directions",
        icon = shiny::icon("compass"),
        footer = shiny::tags$p("More Info", shiny::icon("arrow-alt-circle-right")),
        color = "lightblue",
        gradient = TRUE
      )
    })
    output$app_maintainers <- renderValueBox({
      valueBox(
        subtitle = "",
        value = "Application Maintainers",
        icon = shiny::icon("user"),
        footer = shiny::tags$p("More Info", shiny::icon("arrow-alt-circle-right")),
        color = "indigo",
        gradient = TRUE
      )
    })
    output$security_and_license <- renderValueBox({
      valueBox(
        subtitle = "",
        value = "Security and License",
        icon = shiny::icon("handshake"),
        footer = shiny::tags$p("More Info", shiny::icon("arrow-alt-circle-right")),
        color = "warning",
        gradient = TRUE
      )
    })
  })
}



## To be copied in the server
# mod_welcome_tab_server("welcome_tab_ui_1")
bs4Dash::valueBox(
  subtitle = "",
  value = h2("MIN", 150),
  icon = shiny::icon("check-circle"),
)
