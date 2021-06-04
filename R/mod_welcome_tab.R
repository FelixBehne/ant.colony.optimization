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
        icon = shiny::icon("rocket"),
        size = "lg",
        style = "padding-x:18px;"
      )
    ),
    br(),
    br(),
    shiny::fluidRow(
      shiny::column(
        width = 6,
        bs4Dash::valueBox(
          subtitle = "",
          value = "App Status",
          icon = shiny::icon("check-circle"),
          footer = actionButton(
            inputId = ns("app_status"),
            label = shiny::tags$h6("More Info", shiny::icon("arrow-alt-circle-right")),
            style = "border: none; background-color: transparent; outline: none;"
          ),
          color = "olive",
          gradient = TRUE,
          width = 8
        ),
        br(),
        bs4Dash::valueBox(
          subtitle = "",
          value = "Directions",
          icon = shiny::icon("compass"),
          footer = actionButton(
            inputId = ns("directions"),
            label = shiny::tags$h6("More Info", shiny::icon("arrow-alt-circle-right")),
            style = "border: none; background-color: transparent; outline: none;"
          ),
          color = "lightblue",
          gradient = TRUE,
          width = 8
        )
      ),
      shiny::column(
        width = 6,
        bs4Dash::valueBox(
          subtitle = "",
          value = "App Maintainers",
          icon = shiny::icon("user"),
          footer = actionButton(
            inputId = ns("app_maintainers"),
            label = shiny::tags$h6("More Info", shiny::icon("arrow-alt-circle-right")),
            style = "border: none; background-color: transparent; outline: none;"
          ),
          color = "indigo",
          gradient = TRUE,
          width = 8
        ),
        br(),
        bs4Dash::valueBox(
          subtitle = "",
          value = "Security and License",
          icon = shiny::icon("handshake"),
          footer = actionButton(
            inputId = ns("security_and_license"),
            label = shiny::tags$h6("More Info", shiny::icon("arrow-alt-circle-right")),
            style = "border: none; background-color: transparent; outline: none;"
          ),
          color = "danger",
          gradient = TRUE,
          width = 8
        )
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

    shiny::observeEvent(input$app_status, {
      shinyalert::shinyalert(
        title = "App Status",
        text = "The App Build Was Successfull!
                  Current Build: 0.0.1.9000",
        size = "m",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "success",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#249c24",
        animation = TRUE
      )
    })
    shiny::observeEvent(input$directions, {
      shinyalert::shinyalert(
        title = "Directions",
        text = "If you need any help with using this dashboard please use the info buttons and have fun!",
        size = "m",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "info",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "Thanks, I will!",
        confirmButtonCol = "#249c24",
        animation = TRUE
      )
    })
    shiny::observeEvent(input$app_maintainers, {
      shinyalert::shinyalert(
        title = "App Maintainers",
        text = "This Dashboard has been build by:
                  Sarah Engelmayer, Markus Koch, Moritz Link and Felix Behne",
        size = "m",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "info",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "Ok",
        confirmButtonCol = "#249c24",
        animation = TRUE
      )
    })
    shiny::observeEvent(input$security_and_license, {
      shinyalert::shinyalert(
        title = "Security and License ",
        text = "All the information on this website - https://felixbehne.shinyapps.io/ant-colony-optimization/ - is published in good faith and for general information purpose only. ACO Dashboard does not make any warranties about the completeness, reliability and accuracy of this information. Any action you take upon the information you find on this website (ACO Dashboard), is strictly at your own risk. ACO Dashboard will not be liable for any losses and/or damages in connection with the use of our website. #nolint
        License: MIT
        ",
        size = "m",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "info",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "Ok",
        confirmButtonCol = "#249c24",
        animation = TRUE
      )
    })
  })
}
