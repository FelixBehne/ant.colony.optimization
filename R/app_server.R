#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#'
#' @noRd
app_server <- function(input, output, session) {
  mod_ant_foraging_tab_server("mod_ant_foraging_tab_ui_1")
  mod_algorithm_tab_server("mod_algorithm_tab_ui_1")
  mod_rosenbrock_tab_server(id = "mod_rosenbrock_tab_ui_1", input_g = input)
  mod_himmelblau_tab_server(id = "himmelblau_tab_ui_1", input_g = input)
  mod_ant_generations_tab_server(id = "ant_generations_tab_ui_1", input_g = input)
  mod_tsp_tab_server(id = "tsp_tab_ui_1", input_g = input)
  mod_performance_tab_server(id = "performance_tab_ui_1", input_g = input)
  mod_welcome_tab_server(id = "welcome_tab_ui_1")
  update_controlbar(input, output, session)
}
