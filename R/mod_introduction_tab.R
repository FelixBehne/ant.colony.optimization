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
    shiny::fluidRow(
      
    
      
      bs4Dash::box(
        id = "introduction",
        title = "Grundlagen",
        maximizable = TRUE,
        collapsible = TRUE,
        closable = TRUE,
        width = 12,
        height = "100%",
        includeMarkdown(app_sys("app/www/rmd/history.Rmd"))
      ),
      bs4Dash::box(
        id = "introduction",
        title = "Grundlagen",
        maximizable = TRUE,
        collapsible = TRUE,
        closable = TRUE,
        width = 12,
        height = "100%",
        includeMarkdown(app_sys("app/www/rmd/Arten.Rmd"))
      ),
      bs4Dash::box(
        id = "introduction",
        title = "Grundlagen",
        maximizable = TRUE,
        collapsible = TRUE,
        closable = TRUE,
        width = 12,
        height = "100%",
        includeMarkdown(app_sys("app/www/rmd/Herkunft.Rmd"))
      )
    )
    
    
    
    
  )
}
