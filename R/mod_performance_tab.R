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
          bs4Dash::valueBoxOutput(outputId = ns("aco"), tags$style("#dri {width:400px; height:300px;}"))
        ),
        bs4Dash::box(
          id = "alo_box",
          title = "Ant Lion Optimizer",
          maximizable = TRUE,
          collapsible = TRUE,
          closable = TRUE,
          width = 12,
          height = "100%",
          bs4Dash::valueBoxOutput(outputId = ns("alo"), tags$style("#dri {width:400px; height:300px;}"))
        ),
        bs4Dash::box(
          id = "ba_nox",
          title = "Bat Optimizer",
          maximizable = TRUE,
          collapsible = TRUE,
          closable = TRUE,
          width = 12,
          height = "100%",
          bs4Dash::valueBoxOutput(outputId = ns("ba"), tags$style("#dri {width:400px; height:300px;}"))
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
          bs4Dash::valueBoxOutput(outputId = ns("cso"), tags$style("#dri {width:400px; height:300px;}"))
        ),
        bs4Dash::box(
          id = "da_box",
          title = "Dragonfly Optimizer",
          maximizable = TRUE,
          collapsible = TRUE,
          closable = TRUE,
          width = 12,
          height = "100%",
          bs4Dash::valueBoxOutput(outputId = ns("da"), tags$style("#dri {width:400px; height:300px;}"))
        ),
        bs4Dash::box(
          id = "ffa_box",
          title = "Firefly Optimizer",
          maximizable = TRUE,
          collapsible = TRUE,
          closable = TRUE,
          width = 12,
          height = "100%",
          bs4Dash::valueBoxOutput(outputId = ns("ffa"), tags$style("#dri {width:400px; height:300px;}"))
        )
      )
    )
  )
}

#' performance_tab Server Functions
#'
#' @param input_g global input object fro accessing controlbar.
#'
#' @import bs4Dash shiny metaheuristicOpt
#'
#' @noRd
mod_performance_tab_server <- function(id, input_g) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Create a reactive value for the algorithm results
    results <- reactiveValues()

    # Initialize hashmap with empty value
    results[["aco"]] <- "-"
    results[["alo"]] <- "-"
    results[["ba"]] <- "-"
    results[["cso"]] <- "-"
    results[["da"]] <- "-"
    results[["ffa"]] <- "-"

    shiny::observeEvent(
      eventExpr = input_g$recalculate_performance,
      handlerExpr = {
        # Get the test function
        test_function <- get_test_function(function_name = input_g$test_function_performance, numb_parameters = 1)

        # Calculate the algorithm results

        # Ant Colony Optimization
        aco <- calculate_min(
          test_function = input_g$test_function_performance,
          lower_bound = input_g$lower_bound_performance,
          upper_bound = input_g$upper_bound_performance,
          iterations = input_g$iterations_performance
        )
        results[["aco"]] <- test_function(c(aco[["x1"]], aco[["x1"]]))

        # Ant Lion Optimization
        results[["alo"]] <- test_function(metaheuristicOpt::ALO(
          FUN = test_function,
          optimType = "MIN",
          numVar = 2, # Static because aco calculation is done with two
          numPopulation = input_g$swarm_size_performance,
          maxIter = input_g$iterations_performance,
          rangeVar = matrix(c(input_g$lower_bound_performance, input_g$upper_bound_performance), nrow = 2)
        ))

        # Bat Optimizer
        results[["ba"]] <- test_function(metaheuristicOpt::BA(
          FUN = test_function,
          optimType = "MIN",
          numVar = 2, # Static because aco calculation is done with two
          numPopulation = input_g$swarm_size_performance,
          maxIter = input_g$iterations_performance,
          rangeVar = matrix(c(input_g$lower_bound_performance, input_g$upper_bound_performance), nrow = 2)
        ))

        # Cat Swarm Optimizer
        results[["cso"]] <- test_function(metaheuristicOpt::CSO(
          FUN = test_function,
          optimType = "MIN",
          numVar = 2, # Static because aco calculation is done with two
          numPopulation = input_g$swarm_size_performance,
          maxIter = input_g$iterations_performance,
          rangeVar = matrix(c(input_g$lower_bound_performance, input_g$upper_bound_performance), nrow = 2)
        ))

        # Dragonfly Optimizer
        results[["da"]] <- test_function(metaheuristicOpt::DA(
          FUN = test_function,
          optimType = "MIN",
          numVar = 2, # Static because aco calculation is done with two
          numPopulation = input_g$swarm_size_performance,
          maxIter = input_g$iterations_performance,
          rangeVar = matrix(c(input_g$lower_bound_performance, input_g$upper_bound_performance), nrow = 2)
        ))

        # Firefly Optimizer
        results[["ffa"]] <- test_function(metaheuristicOpt::FFA(
          FUN = test_function,
          optimType = "MIN",
          numVar = 2, # Static because aco calculation is done with two
          numPopulation = input_g$swarm_size_performance,
          maxIter = input_g$iterations_performance,
          rangeVar = matrix(c(input_g$lower_bound_performance, input_g$upper_bound_performance), nrow = 2)
        ))
      }
    )

    output$aco <- bs4Dash::renderValueBox({
      bs4Dash::valueBox(
        subtitle = "",
        value = h2("MIN", results[["aco"]]),
        icon = shiny::icon("bug")
      )
    })
    output$alo <- bs4Dash::renderValueBox({
      bs4Dash::valueBox(
        subtitle = "",
        value = h2("MIN", results[["alo"]]),
        icon = shiny::icon("paw")
      )
    })
    output$ba <- bs4Dash::renderValueBox({
      bs4Dash::valueBox(
        subtitle = "",
        value = h2("MIN:", results[["ba"]]),
        width = 6,
        icon = shiny::icon("dove")
      )
    })
    output$cso <- bs4Dash::renderValueBox({
      bs4Dash::valueBox(
        subtitle = "",
        value = h2("MIN", results[["cso"]]),
        width = NULL,
        icon = shiny::icon("cat")
      )
    })
    output$da <- bs4Dash::renderValueBox({
      bs4Dash::valueBox(
        subtitle = "",
        value = h2("MIN", results[["da"]]),
        width = 6,
        icon = shiny::icon("dragon")
      )
    })
    output$ffa <- bs4Dash::renderValueBox({
      bs4Dash::valueBox(
        subtitle = "",
        value = h2("MIN", results[["ffa"]]),
        width = 6,
        icon = shiny::icon("crow")
      )
    })
    shiny::observeEvent(
      eventExpr = input_g$test_function_info,
      handlerExpr = {
        shinyalert::shinyalert(
          title = "Test Function",
          text = "The test function to use for evaluation the models.",
          size = "s",
          closeOnEsc = TRUE,
          closeOnClickOutside = FALSE,
          html = FALSE,
          type = "info",
          showConfirmButton = TRUE,
          showCancelButton = FALSE,
          confirmButtonText = "OK",
          confirmButtonCol = "#249c24",
          animation = TRUE
        )
      }
    )
    shiny::observeEvent(
      eventExpr = input_g$upper_bound_info,
      handlerExpr = {
        shinyalert::shinyalert(
          title = "Upper Bound",
          text = "The upper boundary of the number space, which limits the search space for the algorithm.",
          size = "s",
          closeOnEsc = TRUE,
          closeOnClickOutside = FALSE,
          html = FALSE,
          type = "info",
          showConfirmButton = TRUE,
          showCancelButton = FALSE,
          confirmButtonText = "OK",
          confirmButtonCol = "#249c24",
          animation = TRUE
        )
      }
    )
    shiny::observeEvent(
      eventExpr = input_g$lower_bound_info,
      handlerExpr = {
        shinyalert::shinyalert(
          title = "Lower Bound",
          text = "The lower boundary of the number space, which limits the search space for the algorithm.",
          size = "s",
          closeOnEsc = TRUE,
          closeOnClickOutside = FALSE,
          html = FALSE,
          type = "info",
          showConfirmButton = TRUE,
          showCancelButton = FALSE,
          confirmButtonText = "OK",
          confirmButtonCol = "#249c24",
          animation = TRUE
        )
      }
    )
    shiny::observeEvent(
      eventExpr = input_g$iterations_info,
      handlerExpr = {
        shinyalert::shinyalert(
          title = "Number of Iterations",
          text = "The maximum number of iterations to use for calculating the minimum.",
          size = "s",
          closeOnEsc = TRUE,
          closeOnClickOutside = FALSE,
          html = FALSE,
          type = "info",
          showConfirmButton = TRUE,
          showCancelButton = FALSE,
          confirmButtonText = "OK",
          confirmButtonCol = "#249c24",
          animation = TRUE
        )
      }
    )
    shiny::observeEvent(
      eventExpr = input_g$swarm_size_info,
      handlerExpr = {
        shinyalert::shinyalert(
          title = "Swarm Size",
          text = "The number of individuals in the population to use for the algorithm.",
          size = "s",
          closeOnEsc = TRUE,
          closeOnClickOutside = FALSE,
          html = FALSE,
          type = "info",
          showConfirmButton = TRUE,
          showCancelButton = FALSE,
          confirmButtonText = "OK",
          confirmButtonCol = "#249c24",
          animation = TRUE
        )
      }
    )
  })
}
