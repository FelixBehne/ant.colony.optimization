#' ACO Implemenatation with the package evoper
#'
#' @param iter
#' @param minim
#' @param maxim
#' @param fu
#'
#' @return
#'
#' @import evoper
#' @noRd
calculate_min <- function(iter = 30, minim = -1, maxim = 1, fu = "rosenbrock") {
  set.seed(161803398)
  if (fu == "rosenbrock") {
    rosenbrock2 <- function(x1, x2) {
      (1 - x1)^2 + 100 * (x2 - x1^2)^2
    }
    objective <- evoper::PlainFunction$new(rosenbrock2)
  }
  else if (fu == "himmelblau") {
    himmelblau2 <- function(x1, x2) {
      (x1^2 + x2 - 11)^2 + (x1 + x2^2 - 7)^2
    }
    objective <- evoper::PlainFunction$new(himmelblau2)
  }

  objective$Parameter(name = "x1", min = minim, max = maxim)
  objective$Parameter(name = "x2", min = minim, max = maxim)

  acor_options <- evoper::OptionsACOR$new()
  acor_options$setValue("iterations", iter)
  system.time(results <- evoper::extremize("acor", objective, acor_options))
  ant_optim_rosenbrock <- data.frame(
    x1 = c(results$getBest()$x1),
    x2 = c(results$getBest()$x2),
    f = c(results$getBest()$fitness)
  )
  return(ant_optim_rosenbrock)
}
