#' Updates the controlbar
#'
#' @description Updates the controlbar according to the currently active tab
#'
#' @param input Input from global server.
#' @param output Output from global server.
#' @param session Session from global server.
#'
#' @return The return value, if any, from executing the function.
#'
#' @import shiny bs4Dash
update_controlbar <- function(input, output, session) {
  return(observeEvent(
    eventExpr = input$visualisations,
    handlerExpr = {
      if (input$visualisations == "himmelblau_plot" || input$visualisations == "rosenbrock_plot") {
        output$controlbar <- bs4Dash::renderMenu({
          bs4Dash::controlbarMenu(
            id = "controlbar_menu",
            bs4Dash::controlbarItem(
              title = "Algorithm",
              shiny::numericInput(
                inputId = "lower_bound_test",
                label = "Lower Bound:",
                value = -1,
                min = -50,
                max = -1,
                step = 1
              ),
              shiny::numericInput(
                inputId = "upper_bound_test",
                label = "Interval Upper Limit:",
                value = 1,
                min = 1,
                max = 50,
                step = 1
              ),
              shiny::sliderInput(
                inputId = "iterations_test",
                label = "Iterations:",
                min = 0,
                max = 120,
                value = 1,
                step = 1
              )
            ),
            bs4Dash::controlbarItem(
              title = "Plot",
              shiny::sliderInput(
                inputId = "phi",
                label = "Vertical Rotation:",
                min = 1,
                max = 300,
                value = 20
              ),
              shiny::sliderInput(
                inputId = "theta",
                label = "Horizontal Rotation:",
                min = 1,
                max = 300,
                value = 150
              ),
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
              shiny::numericInput(
                inputId = "lower_bound_ant_generations",
                label = "Lower Bound:",
                value = -5,
                min = -20,
                max = -1,
                step = 1
              ),
              shiny::numericInput(
                inputId = "upper_bound_ant_generations",
                label = "Upper Bound:",
                value = 5,
                min = 1,
                max = 20,
                step = 1
              ),
              shiny::sliderInput(
                inputId = "gen_numb_ant_generations",
                label = "Number of generations",
                min = 0,
                max = 50,
                value = 1
              ),
              shiny::sliderInput(
                inputId = "hor_numb_ant_generations",
                label = "Number of Ants:",
                min = 1,
                max = 100,
                value = 40
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
              shiny::fluidRow(
                width = 12,
                shiny::column(width = 10, shiny::sliderInput(
                  inputId = "alpha",
                  label = "Alpha:",
                  min = 1,
                  max = 10,
                  value = 5
                )), shiny::column(
                  width = 2,
                  style = "margin-top: 45px;",
                  shinyWidgets::circleButton(
                    inputId = "alpha_info",
                    icon = icon("info"),
                    size = "xs"
                  )
                ),
              ),
              shiny::fluidRow(
                width = 12,
                shiny::column(
                  width = 10,
                  shiny::sliderInput(
                    inputId = "beta",
                    label = "Beta:",
                    min = 1,
                    max = 10,
                    value = 5
                  )
                ),
                shiny::column(
                  width = 2,
                  style = "margin-top: 45px;",
                  shinyWidgets::circleButton(
                    inputId = "beta_info",
                    icon = icon("info"),
                    size = "xs"
                  )
                ),
              ),
              shiny::fluidRow(
                width = 12,
                shiny::column(
                  width = 10,
                  shiny::sliderInput(
                    inputId = "evaporation",
                    label = "Evaporation:",
                    min = 0.1,
                    max = 1.0,
                    value = 0.5
                  )
                ),
                shiny::column(
                  width = 2,
                  style = "margin-top: 45px;",
                  shinyWidgets::circleButton(
                    inputId = "evaporation_info",
                    icon = icon("info"),
                    size = "xs"
                  )
                ),
              ),
              shiny::fluidRow(
                width = 12,
                shiny::column(
                  width = 10,
                  shiny::sliderInput(
                    inputId = "randomness_f",
                    label = "Randomness Factor:",
                    min = 0,
                    max = 10,
                    value = 5
                  )
                ),
                shiny::column(
                  width = 2,
                  style = "margin-top: 45px;",
                  shinyWidgets::circleButton(
                    inputId = "randomness_f_info",
                    icon = icon("info"),
                    size = "xs"
                  )
                ),
              ),
              shiny::fluidRow(
                width = 12,
                shiny::column(
                  width = 10,
                  shiny::sliderInput(
                    inputId = "numb_ants",
                    label = "Number of Ants:",
                    min = 1,
                    max = 50,
                    value = 10
                  )
                ),
                shiny::column(
                  width = 2,
                  style = "margin-top: 45px;",
                  shinyWidgets::circleButton(
                    inputId = "numb_ants_info",
                    icon = icon("info"),
                    size = "xs"
                  )
                ),
              ),
              shiny::fluidRow(
                width = 12,
                shiny::column(
                  width = 10,
                  shiny::sliderInput(
                    inputId = "iterations",
                    label = "Iterations:",
                    min = 1,
                    max = 50,
                    value = 5
                  )
                ),
                shiny::column(
                  width = 2,
                  style = "margin-top: 45px;",
                  shinyWidgets::circleButton(
                    inputId = "iterations_info",
                    icon = icon("info"),
                    size = "xs"
                  )
                )
              ),
              shiny::fluidRow(
                shiny::column(
                  width = 12,
                  align = "center",
                  bs4Dash::actionButton(
                    inputId = "action",
                    icon = shiny::icon("play-circle"),
                    label = "Calculate",
                    status = "success"
                  ),
                )
              )
            )
          )
        })
        bs4Dash::updateControlbar(id = "controlbar", session = session)
      }
      else if (input$visualisations == "performance") {
        output$controlbar <- bs4Dash::renderMenu({
          bs4Dash::controlbarMenu(
            id = "controlbar_menu",
            bs4Dash::controlbarItem(
              title = "Algorithm",
              shiny::fluidRow(
                width = 12,
                shiny::column(
                  width = 10,
                  selectInput(
                    inputId = "test_function_performance",
                    label = "Test Function",
                    choices = c("Himmelblau" = "himmelblau", "Rosenbrock" = "rosenbrock"),
                    selected = "himmelblau",
                    multiple = FALSE,
                  )
                ), shiny::column(
                  width = 2,
                  style = "margin-top: 45px;",
                  shinyWidgets::circleButton(
                    inputId = "test_function_performance_info",
                    icon = icon("info"),
                    size = "xs"
                  )
                ),
              ),
              shiny::fluidRow(
                width = 12,
                shiny::column(
                  width = 10,
                  shiny::sliderInput(
                    inputId = "upper_bound_performance",
                    label = "Upper Bound",
                    min = 0,
                    max = 50,
                    value = 10
                  )
                ),
                shiny::column(
                  width = 2,
                  style = "margin-top: 45px;",
                  shinyWidgets::circleButton(
                    inputId = "upper_bound_performance_info",
                    icon = icon("info"),
                    size = "xs"
                  )
                ),
              ),
              shiny::fluidRow(
                width = 12,
                shiny::column(
                  width = 10,
                  shiny::sliderInput(
                    inputId = "lower_bound_performance",
                    label = "Lower Bound",
                    min = 0,
                    max = -50,
                    value = -10
                  )
                ),
                shiny::column(
                  width = 2,
                  style = "margin-top: 45px;",
                  shinyWidgets::circleButton(
                    inputId = "lower_bound_performance_info",
                    icon = icon("info"),
                    size = "xs"
                  )
                ),
              ),
              shiny::fluidRow(
                width = 12,
                shiny::column(
                  width = 10,
                  shiny::sliderInput(
                    inputId = "iterations_performance",
                    label = "Number of Iterations:",
                    min = 0,
                    max = 120,
                    value = 10
                  )
                ),
                shiny::column(
                  width = 2,
                  style = "margin-top: 45px;",
                  shinyWidgets::circleButton(
                    inputId = "iterations_performance_info",
                    icon = icon("info"),
                    size = "xs"
                  )
                ),
              ),
              shiny::fluidRow(
                width = 12,
                shiny::column(
                  width = 10,
                  shiny::sliderInput(
                    inputId = "swarm_size_performance",
                    label = "Swarm Size:",
                    min = 1,
                    max = 100,
                    value = 20
                  )
                ),
                shiny::column(
                  width = 2,
                  style = "margin-top: 45px;",
                  shinyWidgets::circleButton(
                    inputId = "swarm_size_performance_info",
                    icon = icon("info"),
                    size = "xs"
                  )
                )
              ),
              br(),
              br(),
              shiny::fluidRow(
                shiny::column(
                  width = 12,
                  align = "center",
                  bs4Dash::actionButton(
                    inputId = "recalculate_performance",
                    icon = shiny::icon("play"),
                    label = "Realculate",
                    status = "success"
                  )
                )
              )
            )
          )
        })
        bs4Dash::updateControlbar(id = "controlbar", session = session)
      } else {
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
    },
    ignoreNULL = FALSE
  ))
}
