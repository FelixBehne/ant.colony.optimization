#' performance_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @import shiny bs4Dash htmltools
#'
#' @noRd
mod_performance_tab_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::titlePanel("Performance"),
    htmltools::br(),
    shiny::fluidRow(
      width = 12,
      shiny::column(
        6,
        bs4Dash::box(
          id = "aco_box",
          title = "Ant Colony Optimization",
          maximizable = TRUE,
          collapsible = TRUE,
          closable = TRUE,
          width = 12,
          height = "100%",
          bs4Dash::infoBoxOutput(outputId = ns("aco"), tags$style("#dri {width:400px; height:300px;}"))
        ),
        bs4Dash::box(
          id = "alo_box",
          title = "Ant Lion Optimizer",
          maximizable = TRUE,
          collapsible = TRUE,
          closable = TRUE,
          width = 12,
          height = "100%",
          bs4Dash::infoBoxOutput(outputId = ns("alo"), tags$style("#dri {width:400px; height:300px;}"))
        ),
        bs4Dash::box(
          id = "ba_nox",
          title = "Bat Optimizer",
          maximizable = TRUE,
          collapsible = TRUE,
          closable = TRUE,
          width = 12,
          height = "100%",
          bs4Dash::infoBoxOutput(outputId = ns("ba"), tags$style("#dri {width:400px; height:300px;}"))
        )
      ),
      shiny::column(
        6,
        bs4Dash::box(
          id = "cso_box",
          title = "Cat Swarm Optimizer",
          maximizable = TRUE,
          collapsible = TRUE,
          closable = TRUE,
          width = 12,
          height = "100%",
          bs4Dash::infoBoxOutput(outputId = ns("cso"), tags$style("#dri {width:400px; height:300px;}"))
        ),
        bs4Dash::box(
          id = "da_box",
          title = "Dragonfly Optimizer",
          maximizable = TRUE,
          collapsible = TRUE,
          closable = TRUE,
          width = 12,
          height = "100%",
          bs4Dash::infoBoxOutput(outputId = ns("da"), tags$style("#dri {width:400px; height:300px;}"))
        ),
        bs4Dash::box(
          id = "ffa_box",
          title = "Firefly Optimizer",
          maximizable = TRUE,
          collapsible = TRUE,
          closable = TRUE,
          width = 12,
          height = "100%",
          bs4Dash::infoBoxOutput(outputId = ns("ffa"), tags$style("#dri {width:400px; height:300px;}"))
        )
      )
    )
  )
}

#' performance_tab Server Functions
#'
#' @import bs4Dash shiny
#'
#' @noRd
mod_performance_tab_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$aco <- renderInfoBox({
      valueBox(
        subtitle = "",
        value = h2("MIN", 150),
        width = 6,
        icon = shiny::icon("bug")
      )
    })
    output$alo <- bs4Dash::renderInfoBox({
      bs4Dash::valueBox(
        subtitle = "",
        value = h2("MIN", 150),
        icon = shiny::icon("paw"),
      )
    })
    output$ba <- bs4Dash::renderInfoBox({
      bs4Dash::valueBox(
        subtitle = "",
        value = h2("MIN:", 150),
        width = 6,
        icon = shiny::icon("dove"),
      )
    })
    output$cso <- bs4Dash::renderValueBox({
      bs4Dash::valueBox(
        subtitle = "",
        value = h2("MIN", 150),
        width = NULL,
        icon = shiny::icon("cat"),
      )
    })
    output$da <- bs4Dash::renderValueBox({
      bs4Dash::valueBox(
        subtitle = "",
        value = h2("MIN", 150),
        width = 6,
        icon = shiny::icon("dragon"),
      )
    })
    output$ffa <- bs4Dash::renderValueBox({
      bs4Dash::valueBox(
        subtitle = "",
        value = h2("MIN", 150),
        width = 6,
        icon = shiny::icon("crow"),
      )
    })
  })
}
