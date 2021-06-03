
#---Himmelblau ------------------------------------

#' Himmelblau function
himmelblau <- function(x) {
  x1 <- x[1]
  x2 <- x[2]
  (x1^2 + x2 - 11)^2 + (x1 + x2^2 - 7)^2
}

#' Gradient of the Himmelblau function
gradient_himmelblau <- function(x) {
  x1 <- x[1]
  x2 <- x[2]
  c(
    4 * x1 * (x1^2 + x2 - 11) + 2(x1 + x2^2 - 7),
    4 * x2 * (x2^2 + x1 - 7) + 2 * (x2 + x1^2 - 11)
  )
}

#' Minimize Himmelblau function
#'
#' @details If convergence=0 function converges; 4 local mimima and a global minimum at x1=-3.77, x2=-3.28, f=0
opt_himmelblau <- optim(c(-5, 5), himmelblau)

minima_himmelblau <- data.frame(
  x1 = c(opt_himmelblau$par[1], 3, -3.78, 3.58),
  x2 = c(opt_himmelblau$par[2], 2, -3.28, -1.85),
  f = c(opt_himmelblau$value, 0, 0, 0)
)


#---Rosenbrock ------------------------------------

#' Rosenbrock function
rosenbrock <- function(x) {
  x1 <- x[1]
  x2 <- x[2]
  100 * (x2 - x1 * x1)^2 + (1 - x1)^2
}

#' Gradient of the Rosenbrock function
gradient_rosenbrock <- function(x) {
  x1 <- x[1]
  x2 <- x[2]
  c(
    -400 * x1 * (x2 - x1^2) - 2 * (1 - x1), # first derivative after x1
    200 * (x2 - x1^2)
  ) # # first derivative after x2
}

#' Minimize Rosenbrock function
#'
#' @details If convergence=0 function converges; Mimimum at x1=1, x2=1, f=0
opt_rosenbrock <- optim(c(-1.2, 1), rosenbrock)

minima_rosenbrock <- data.frame(
  x1 = c(opt_rosenbrock$par[1]),
  x2 = c(opt_rosenbrock$par[2]),
  f = c(opt_rosenbrock$value)
)

#---Plotting Utils ------------------------------------

#' Generate 3d function for a given objective function
#'
#' @param fu the function to plot for
#' @param minim Minimum limit of the axes
#' @param maxim Maximum limit of the axes
#' @param theta_input Angle defining the viewing direction. Theta gives the degree of the vertical rotation.
#' @param phi_input Angle defining the viewing direction. Phi gives the degree of the horizontal rotation.
#' @param shade_inputValues Values of shade close to one yield shading similar to a point light source model and values close to zero
#'    produce no shading. Values in the range 0.5 to 0.75 provide an approximation to daylight illumination.
#' @param colour The color(s) of the surface facets._input
return_3d_plot <- function(fu = "rosenbrock", minim = -1, maxim = 1, theta_input = "150", phi_input = "20", shade_input = 0.3, colour = "green") {
  if (fu == "rosenbrock") {
    fun <- function(x, y) {
      (1 - x)^2 + 100 * (y - x^2)^2
    }
  } else if (fu == "himmelblau") {
    fun <- function(x, y) {
      (x^2 + y - 11)^2 + (x + y^2 - 7)^2
    }
  }
  x <- y <- seq(minim, maxim, length = 20)
  z <- outer(x, y, fun)
  persp(x, y, z,
    theta = theta_input, phi = phi_input,
    shade = shade_input, col = colour,
  )
}

#---Plot Generations of ants on Himmelblau function --------------------

#' Cost function of Himmelblau Function
#'
#' @param param_list List of Parameters.
cost_function_himmelblau <- function(param_list) {
  x1 <- param_list[1]
  x2 <- param_list[2]
  (x1^2 + x2 - 11)^2 + (x1 + x2^2 - 7)^2
}

#' Creates a start set
#'
#' @param number_ants number of ants of each generation.
#' @param start_interval the range of the x, y and f value in which we search the minimum; the interval of the initial locations of the ants
#' @return a list of the x-, y- and f- value of each ant which represents the locations of the ants
make_start_set <- function(number_ants, start_interval) {
  set.seed(120)
  gen_p <- rand_param(param_list = start_interval, hor = number_ants)
  return(gen_p)
}

get_first_generation_with_f <- function(datos = "NA", gen_p, cost_f, paralelo = 0) {
  err_gen <- calc_err(datos, cost_f, gen_p, paralelo = paralelo)
  xyf <- c(gen_p, err_gen)
  return(xyf)
}


#' Calculates an iteration step,  calculate Values for first generation (use Algorithm-Functions from File "fct_aco.R")
#'
#' @param datos the data of interest (this parameter is not necessary for us)
#' @param cost_f the objective function
#' @return a new list of the x-, y- and f- value of each ant which represents the new locations of the ants
calc_gens <- function(datos = "NA", cost_f, param_list, gen_p, gen, q = 0.2, eps = 0.5, paralelo = 0) {
  print(param_list)
  while (gen > 0) {
    err_gen <- calc_err(datos, cost_f, gen_p, paralelo = paralelo)
    pesos_gen <- pesos(err_gen, q)
    prob_gen <- prob_hor(pesos_gen)
    desv <- c_sigma(gen_p, eps)
    gen_p <- new_gen(gen_p, desv, prob_gen, param_list)
    gen <- gen - 1
  }
  xyf <- c(gen_p, err_gen)
  return(xyf)
}

#' Prepares the data with the ants' locations for plotting
#' @param hor_number Number of ants.
#' @param xyf the list of the x-, y- and f- value of each ant which represents the locations of the ants
prepare_for_plot <- function(hor_number, xyf) {

  # Add column for colour
  vector_for_color <- rep("ants", hor_number) # create a vector with length hor (number of ants) and a random string-value that determines
  # that the ant values are displayed in the same colour.
  xyf$colour <- vector_for_color

  # Add first minimum of Himmelblau
  xyf$x <- c(xyf$x, -2.80)
  xyf$y <- c(xyf$y, 3.13)
  xyf$f <- c(xyf$f, 0.00)
  xyf$colour <- c(xyf$colour, "min") # give the actual minima a string-value that differs from the string-value of the ant-values
  # in order to give them a different colour

  # Add second minimum of Himmelblau
  xyf$x <- c(xyf$x, 3.00)
  xyf$y <- c(xyf$y, 2.00)
  xyf$f <- c(xyf$f, 0.00)
  xyf$colour <- c(xyf$colour, "min")

  # Add third minimum of Himmelblau
  xyf$x <- c(xyf$x, -3.78)
  xyf$y <- c(xyf$y, -3.28)
  xyf$f <- c(xyf$f, 0.00)
  xyf$colour <- c(xyf$colour, "min")

  # Add fourth minimum of Himmelblau
  xyf$x <- c(xyf$x, 3.58)
  xyf$y <- c(xyf$y, -1.85)
  xyf$f <- c(xyf$f, 0.00)
  xyf$colour <- c(xyf$colour, "min")

  # Calculate mean values of ants
  mean_f <- sum(xyf$f) / hor_number
  # assign("meanF", meanF, envir = .GlobalEnv) #try to assign a value to a global variable
  mean_x1 <- sum(xyf$x) / hor_number
  mean_x2 <- sum(xyf$y) / hor_number

  # Add mean values of ants
  xyf$x <- c(xyf$x, mean_x1)
  xyf$y <- c(xyf$y, mean_x2)
  xyf$f <- c(xyf$f, mean_f)
  xyf$colour <- c(xyf$colour, "mean") # give the mean values of the ants a string-value that differs from the ones above in order
  # to give them a third different colour

  return(xyf)
}

#' Actual minima of Himmelblau Function in a data frame
minima_himmelblau2 <- data.frame(
  x = c(opt_himmelblau$par[1], 3, -3.78, 3.58),
  y = c(opt_himmelblau$par[2], 2, -3.28, -1.85),
  f = c(opt_himmelblau$value, 0, 0, 0)
)
