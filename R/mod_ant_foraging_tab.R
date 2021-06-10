#' ant_foraging_tab UI Function
#' 
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @import shiny bs4Dash slickR shinycssloaders
#'
#' @noRd
mod_ant_foraging_tab_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::titlePanel("Ant Foraging"),
    bs4Dash::box(
      id = "result_actual",
      maximizable = TRUE,
      collapsible = TRUE,
      closable = TRUE,
      width = 12,
      height = 600,
      dropdownMenu = bs4Dash::boxDropdown(
        icon = shiny::icon("info"),
        bs4Dash::boxDropdownItem(
          id = ns("phase_1"),
          "Phase 1",
        ),
        bs4Dash::boxDropdownItem(
          id = ns("phase_2"),
          "Phase 2",
        ),
        bs4Dash::boxDropdownItem(
          id = ns("phase_3"),
          "Phase 3",
        )
      ),
      shiny::column(12, align = "center", shinycssloaders::withSpinner(slickR::slickROutput(
        outputId = ns("slickr"),
        height = 500,
        width = "100%"
      )))
    )
  )
}

#' ant_foraging_tab Server Functions
#'
#' @import shiny slickR shinyWidgets
#'
#' @noRd
mod_ant_foraging_tab_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns # nolint
    output$slickr <- slickR::renderSlickR({
      imgs <- list.files(app_sys("app/www/img/antSlideShow/"), pattern = ".png", full.names = TRUE) # nolint
      slickR::slickR(imgs)
    })

    shiny::observeEvent(input$phase_1, {
      shinyWidgets::sendSweetAlert(
        session,
        title = "Phase 1",
        text = "The ants run in different directions to look for food. In doing so, they emit pheromones (sexual attractants).",
        btn_colors = "green"
      )
    })

    shiny::observeEvent(input$phase_2, {
      shinyWidgets::sendSweetAlert(
        session,
        title = "Phase 2",
        text = "The ants orientate themselves to the path with the most pheromone traces and take this path to get food.",
        btn_colors = "green"
      )
    })

    shiny::observeEvent(input$phase_3, {
      shinyWidgets::sendSweetAlert(
        session,
        title = "Phase 3",
        text = "The ants form a so-called ant trail and keep this trail even if a new trail is added that is not as long as the ant trail. as the ant trail.", # nolintr
        btn_colors = "green"
      )
    })
  }, )
}
