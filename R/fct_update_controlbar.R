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
              shiny::sliderInput(
                inputId = "interval_min",
                label = "Interval Lower Limit:",
                value = -1,
                min = -50,
                max = -1,
                step = 1
              ),
              shiny::sliderInput(
                inputId = "interval_max",
                label = "Interval Upper Limit:",
                value = 1,
                min = 1,
                max = 50,
                step = 1
              ),
              shiny::sliderInput(
                inputId = "iterations",
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
             # shiny::sliderInput(
            #    inputId = "iterations",
            #    label = "Iterations:",
            #    min = 0,
            #    max = 120,
            #    value = 1,
            #    step = 1
            #  ),
              shiny::sliderInput(
                inputId = "hor_numb",
                label = "Number of Ants:",
                min = 1,
                max = 100,
                value = 40
              ),
              shiny::sliderInput(
                inputId = "gen_numb",
                label = "Number of generations/iterations",
                min = 0,
                max = 50,
                value = 1
              ),
              br(),
              shiny::column(
                width = 12,
                align = "center",
                bs4Dash::actionButton(
                  inputId = "start_gen",
                  label = "Recalculate",
                  icon = shiny::icon("play"),
                  status = "success",
                  width = "150px"
                )
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
                value = 10
              ),
              shiny::sliderInput(
                inputId = "iterations",
                label = "Iterations:",
                min = 1,
                max = 50,
                value = 5
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
  ))
}
