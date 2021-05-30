#' introduction_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @import htmltools
#' @importFrom shiny NS tagList
mod_introduction_tab_ui <- function(id) {
  ns <- NS(id) # nolint
  tagList(
    htmltools::includeMarkdown(app_sys("app/www/rmd/introduction.Rmd"))
  )
}
