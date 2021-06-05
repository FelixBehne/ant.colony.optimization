#' ACO Implemenatation with the package evoper
#'
#' @param iterations number of iterations (= generations)
#' @param lower_bound the minimum limit of the range in which we search the minimum (for the x1, x2, f value)
#' @param upper_bound the maximum limit of the range in which we search the minimum (for the x1, x2, f value)
#' @param test_function the objective function to minimize
#'
#' @return the calculated minimum (x1, x2 and f value)
#'
#' @import evoper
calculate_min <- function(iterations, lower_bound, upper_bound, test_function) {
  set.seed(161803398)
  # Check what test function to use
  objective <- evoper::PlainFunction$new(get_test_function(function_name = test_function, numb_parameters = 2))

  # Set bounds for calculation
  objective$Parameter(name = "x1", min = lower_bound, max = upper_bound)
  objective$Parameter(name = "x2", min = lower_bound, max = upper_bound)

  # Set aco options
  acor_options <- evoper::OptionsACOR$new()
  acor_options$setValue("iterations", iterations)
  system.time(results <- evoper::extremize(type = "acor", objective = objective, options = acor_options))

  # Extract the result into a dataframe
  results_df <- data.frame(
    x1 = c(results$getBest()$x1),
    x2 = c(results$getBest()$x2),
    f = c(results$getBest()$fitness)
  )
  return(results_df)
}
