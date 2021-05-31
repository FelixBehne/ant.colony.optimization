#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#'
#' @noRd
# nolint start
app_server <- function(input, output, session) {
  mod_ant_foraging_tab_server("mod_ant_foraging_tab_ui_1")
  mod_algorithm_tab_server("mod_algorithm_tab_ui_1")
  mod_rosenbrock_tab_server(id = "mod_rosenbrock_tab_ui_1", input_c = input)
  mod_himmelblau_tab_server(id = "himmelblau_tab_ui_1", input_c = input)
  mod_ant_generations_tab_server(id = "ant_generations_tab_ui_1", input_c = input)
  mod_tsp_tab_server(id = "tsp_tab_ui_1", input_c = input)
  mod_performance_tab_server("performance_tab_ui_1")
  observeEvent(
    eventExpr = input$visualisations,
    handlerExpr = {
      if (input$visualisations == "himmelblau_plot" || input$visualisations == "rosenbrock_plot") {
        output$controlbar <- bs4Dash::renderMenu({
          bs4Dash::controlbarMenu(
            id = "controlbar_menu",
            bs4Dash::controlbarItem(
              title = "ACO Config",
              # Slider to choose the minimum value of the area in which we search the minimum of the objective function (equal for x1, x2 and f)
              shiny::sliderInput(
                inputId = "interval_min",
                label = "Interval Lower Limit:",
                value = -1,
                min = -50,
                max = -1,
                step = 1
              ),
              # Slider to choose the maximum value of the area in which we search the minimum of the objective function (equal for x1, x2 and f)
              shiny::sliderInput(
                inputId = "interval_max",
                label = "Interval Upper Limit:",
                value = 1,
                min = 1,
                max = 50,
                step = 1
              ),
              # Slider to choose the number of iterations for the algorithm 
              shiny::sliderInput(
                inputId = "iterations",
                label = "Iterations:",
                min = 0,
                max = 120,
                value = 1,
                step = 1
              )
            ),
            # Slider to choose the degree of the vertical rotation of the 3d plot of the objective function
            bs4Dash::controlbarItem(
              title = "Plot Config",
              shiny::sliderInput(
                inputId = "phi",
                label = "Vertical Rotation:",
                min = 1,
                max = 300,
                value = 20
              ),
              # Slider to choose the degree of the horizontal rotation of the 3d plot of the objective function
              shiny::sliderInput(
                inputId = "theta",
                label = "Horizontal Rotation:",
                min = 1,
                max = 300,
                value = 150
              ),
              # Slider to choose the intensity of the shade of the 3d plot of the objective function (the higher the value the darker the plot is shaded)
              shiny::sliderInput(
                inputId = "shade",
                label = "Shade:",
                min = 0,
                max = 1,
                value = 0.3
              )
            )
          )
        })
        bs4Dash::updateControlbar(id = "controlbar", session = session)
      }
      else if (input$visualisations == "ant_generations_plot") {
        output$controlbar <- bs4Dash::renderMenu({
          bs4Dash::controlbarMenu(
            id = "controlbar_menu",
            bs4Dash::controlbarItem(
              title = "ACO Config",
              shiny::sliderInput(
                inputId = "lower_bound",
                label = "Interval Lower Limit:",
                value = -5,
                min = -20,
                max = -1,
                step = 1
              ),
              shiny::sliderInput(
                inputId = "upper_bound",
                label = "Interval Upper Limit:",
                value = 5,
                min = 1,
                max = 20,
                step = 1
              ),
              shiny::sliderInput(
                inputId = "iterations",
                label = "Iterations:",
                min = 0,
                max = 120,
                value = 1,
                step = 1
              ),
              shiny::sliderInput(
                inputId = "hor_numb",
                label = "Number of Ants:",
                min = 1,
                max = 100,
                value = 40
              ),
              shiny::sliderInput(
                inputId = "gen_numb",
                label = "Number of Generationen",
                min = 0,
                max = 50,
                value = 1
              ),
              br(),
              shiny::column(
                width = 12,
                align = "center",
                bs4Dash::actionButton(inputId = "start_gen", label = "Recalculate", icon = shiny::icon("play"), status = "success", width = "150px")
              )
            )
          )
        })
        bs4Dash::updateControlbar(id = "controlbar", session = session)
      }
      else if (input$visualisations == "tsp_plot") {
        output$controlbar <- bs4Dash::renderMenu({
          bs4Dash::controlbarMenu(
            id = "controlbar_menu",
            bs4Dash::controlbarItem(
              title = "Settings",
              shiny::sliderInput(
                inputId = "alpha",
                label = "Alpha:",
                min = 1,
                max = 10,
                value = 5
              ),
              shiny::sliderInput(
                inputId = "beta",
                label = "Beta:",
                min = 1,
                max = 10,
                value = 5
              ),
              shiny::sliderInput(
                inputId = "evaporation",
                label = "Evaporation:",
                min = 0.1,
                max = 1.0,
                value = 0.5
              ),
              shiny::sliderInput(
                inputId = "randomness_f",
                label = "Randomnessfactor:",
                min = 0,
                max = 10,
                value = 5
              ),
              shiny::sliderInput(
                inputId = "numb_ants",
                label = "Number of Ants:",
                min = 1,
                max = 50,
                value = 30
              ),
              shiny::sliderInput(
                inputId = "iterations",
                label = "Iterations:",
                min = 1,
                max = 50,
                value = 30
              ),
              bs4Dash::actionButton(
                inputId = "action",
                label = "Action"
              ),
              bs4Dash::actionButton(
                inputId = "info",
                label = "Info"
              )
            )
          )
        })
      }
      else if (input$visualisations == "performance") {
        output$controlbar <- bs4Dash::renderMenu({
          bs4Dash::controlbarMenu(
            id = "controlbar_menu",
            bs4Dash::controlbarItem(
              title = "Algorithm",
              selectInput(
                inputId = "test_function",
                label = "Test Function",
                choices = c("Himmelblau" = "himmelblau", "Rosenbrock" = "rosenbrock"),
                selected = "himmelblau",
                multiple = FALSE,
              ),
              shiny::numericInput(
                inputId = "upper_bound",
                label = "Upper Bound",
                min = 0,
                max = 50,
                value = 1
              ),
              shiny::numericInput(
                inputId = "lower_bound",
                label = "Lower Bound",
                min = 0,
                max = 50,
                value = 1
              ),
              shiny::sliderInput(
                inputId = "iterations",
                label = "Number of Iterations:",
                min = 0,
                max = 120,
                value = 1,
                step = 1
              ),
              shiny::sliderInput(
                inputId = "swarm_size",
                label = "Swarm Size (count):",
                min = 1,
                max = 100,
                value = 40
              ),
              br(),
              br(),
              shiny::column(
                width = 12,
                align = "center",
                bs4Dash::actionButton(inputId = "start_gen", label = "Recalculate", icon = shiny::icon("play"), status = "success", width = "150px")
              )
            )
          )
        })
        bs4Dash::updateControlbar(id = "controlbar", session = session)
      }
      else {
        output$controlbar <- bs4Dash::renderMenu({
          bs4Dash::controlbarMenu(
            id = "controlbar_menu",
            bs4Dash::controlbarItem(
              title = "Settings",
              br()
            )
          )
        })
      }
    }
  )
}
# nolint end
