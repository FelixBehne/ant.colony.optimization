# ACO Implementation used for plotting generations of ants
# Adopted from:
# https://rpubs.com/gingersling/ACO#:~:text=The%20algorithm%20is%20a%20method,the%20behavior%20of%20the%20ants.&text=From%20generation%20to%20generation%20the,weight%20based%20on%20different%20function. # nolint


#' Sets first generation of ants randomly
#'
#' @param param_list the range of the x, y and f value in which we search the minimum
#' @param hor number of ants
#'
#' @return a list of the x and y value of each ant of the generation
#' @noRd
rand_param <- function(param_list, hor) {
  res <- data.frame(matrix(ncol = 2, nrow = 0)) # Make result variable
  col_names <- c("x", "y") # Add  col_names
  colnames(res) <- col_names
  for (i in 1:hor) {
    for (j in 1:dim(param_list)[2]) {
      res[i, j] <- runif(1, param_list[1, j], param_list[2, j])
    }
  }
  res
}

#' Calculate f-value ("error") of each ant based on the objective function
#'
#' @param datos the data of interest (this parameter is not necessary for us) 
#' @param cost_f the objective function e.g. himmelblau function
#' @param param_list a list of the ants' x1 and x2 values
#' @param paralelo 1 if the errors should be calculated in parallel
#'
#' @import parallel foreach
#'
#' @return a list of the f-values (errors) of each ant of the generation
#' @noRd
calc_err <- function(datos, cost_f, param_list, paralelo) {
  res <- data.frame(err = numeric())
  if (paralelo == 0) {
    coste <- match.fun(cost_f)
    t_param <- as.data.frame(t(param_list))
    f <- unlist(lapply(t_param, function(x) {
      coste(param_list = x)
    }))
    res <- as.data.frame(f)
    return(res)
  }
  if (paralelo == 1) {
    no_cores <- parallel::detectCores() - 1
    cl <- parallel::makeCluster(no_cores)
    coste <- match.fun(cost_f)
    t_param <- as.data.frame(t(param_list))
    cl <- parallel::makeCluster(no_cores)
    parallel::registerDoParallel(cl)
    f <- foreach::foreach(param_list1 = t_param, .combine = c) %dopar% {
      library(caret)
      cost_f(datos = datos, param_list = param_list1)
    }
    parallel::stopCluster(cl)
    res <- as.data.frame(f)
    res
  }
  res
}

#' Calculate the weight of each ant based on the gaussian distribution
#'
#' @param err the calculated list of errors of the ants
#' @param q  meta-parameter, with a range of (0,1). If q is close to 0 it will give more relevance to better ants,
#'  on the other hand, if its closer to 1 it will distribute the weight between all the ants more equitatively
#'
#' @return a list with the ants' weights
#' @noRd
pesos <- function(err, q) {
  tot <- dim(err)[1]
  res <- data.frame(w = numeric())
  for (i in 1:dim(err)[1]) {
    res[i, ] <- (1 / (q * tot * sqrt(2 * pi))) * exp(-(which(err[i, ] == err[order(err), ])[1] - 1)^(2) / (2 * (q * tot)^(2)))
  }
  res
}
#' Calculates the probability
#'
#' @param peso the list of the calculated ants' weights
#' @return a list of the probabilities of choosing each ant as a prototype for the next generation
#' @noRd
prob_hor <- function(peso) {
  res <- peso
  tot <- sum(peso[, 1])
  res <- res / tot
  res
}

#' Calculates Sigma (standard deviation) of each ant
#'
#' @param param_list a list of the x and y values of the ants of the current generation
#' @param eps represents the symmetry to the expected value; has a range of (0,1); if eps = 0.5 the distribution is symmetric
#'
#' @return a list with the standard deviations of the x and y values of each ant
#' @noRd
c_sigma <- function(param_list, eps) {
  hor <- dim(param_list)[1]
  res <- param_list
  for (i in 1:hor) {
    for (j in 1:dim(param_list)[2]) {
      des <- 0
      for (h in 1:hor) {
        des <- des + abs(param_list[i, j] - param_list[h, j])
      }
      res[i, j] <- eps * des / (hor - 1)
    }
  }
  res
}

#' Calculate the new generation of ants
#' @param param_list the old generation of ants
#' @param desv the calculated standard deviation of each ants x and y values
#' @param param_list_r the range of the x, y and f value in which we search the minimum
#'
#' @return a list of the x and y value of each ant of the generation
#' @noRd
new_gen <- function(param_list, desv, prob, param_list_r) {
  res <- param_list
  hor <- dim(param_list)[1] # Anzahl der x1 Werte
  pars <- dim(param_list)[2] # Anzahl der x2-Werte
  r_num <- matrix(abs(rnorm(hor * pars, 1, 0.1)), ncol = pars)
  aux_r <- matrix(sample(c(-1, 1), size = hor * pars, replace = TRUE), ncol = pars)
  r_num <- r_num * aux_r
  for (i in 1:dim(param_list)[2]) {
    idx <- sample(1:hor, size = hor, replace = TRUE, prob = prob[, 1])
    res[, i] <- param_list[idx, i] + desv[idx, i] * r_num[, i]
    res[, i] <- ifelse(res[, i] < param_list_r[1, i], param_list_r[1, i], res[, i])
    res[, i] <- ifelse(res[, i] > param_list_r[2, i], param_list_r[2, i], res[, i])
  }
  res
}
