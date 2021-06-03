#' Customizes the theme
#'
#' @description Creates a customizes theme with the help of the fresh package
#'
#' @import fresh
customize_theme <- function() {
  create_theme(
    bs4dash_status(
      primary = "#30a062"
    )
  )
}
