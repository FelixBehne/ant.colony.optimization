#' introduction_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @import shiny bs4Dash htmltools
#'
#' @noRd
mod_introduction_tab_ui <- function(id) {
  ns <- shiny::NS(id) # nolint
  shiny::tagList(
    shiny::fluidRow(
      bs4Dash::box(
        id = "introduction",
        title = "Grundlagen",
        maximizable = TRUE,
        collapsible = TRUE,
        closable = TRUE,
        width = 12,
        height = "100%",
        htmltools::includeMarkdown(app_sys("app/www/rmd/history.Rmd"))
      ),
      bs4Dash::box(
        id = "introduction",
        title = "Grundlagen",
        maximizable = TRUE,
        collapsible = TRUE,
        closable = TRUE,
        width = 12,
        height = "100%",
        htmltools::includeMarkdown(app_sys("app/www/rmd/species.Rmd"))
      ),
      bs4Dash::box(
        id = "introduction",
        title = "Grundlagen",
        maximizable = TRUE,
        collapsible = TRUE,
        closable = TRUE,
        width = 12,
        height = "100%",
        htmltools::includeMarkdown(app_sys("app/www/rmd/origin.Rmd"))
      )
    )
  )
}
