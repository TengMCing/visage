# evaluate_p_value --------------------------------------------------------

#' Evaluate test for given p-value and significance level
#'
#' This function reject the null hypothesis if the p-value is smaller than the
#' given significance level plus the tolerance.
#'
#' @param p_value Numeric. A vector of p-values.
#' @param significance_level Numeric. The significance level.
#' @param tol Numeric. Tolerance.
#' @return A vector of Boolean values indicating whether the null hypotheses
#' should be rejected.
#'
#' @examples
#'
#' evaluate_p_value(c(0.05, 0.0501), 0.05, 1e-3)
#' evaluate_p_value(c(0.05, 0.0501), 0.05, 1e-5)
#'
#' @export
evaluate_p_value <- function(p_value, significance_level = 0.05, tol = 1e-6) {
  return(p_value < significance_level + tol)
}


# sim_dist ----------------------------------------------------------------

#' Approximate the distribution of number of detections of a lineup with simulation
#'
#' This function approximate the distribution of number of detections of a
#' lineup for given number of evaluations, selections in each evaluation and
#' plots in a lineup.
#'
#' For a given lineup, plots are assumed to have weights
#' \eqn{W_i, i = 1, ..., N,} where \eqn{N} is the number of plots, and
#' \eqn{W_i} follows a uniform distribution independently. For each draw,
#' weights for a lineup will be simulated. Then, for each evaluation of a draw,
#' the function will sample same number of plots as the number of selection in
#' the evaluation using the simulated weights without replacement.
#' Finally, the distribution of the occurrences of plot 1 in a draw is the
#' approximated distribution of number of detections of a lineup.
#'
#' @param n_evaluations Integer. Number of evaluations.
#' @param n_selections Integer. A vector of the number of selections.
#' @param n_plots Integer. Number of plots in the lineup.
#' @param n_simulations Integer. Number of simulations draws.
#' @return A named vector representing the probability mass function of the distribution.
#'
#' @examples
#' sim_dist(3, c(2,2,3))
#' sim_dist(1, c(1))
#'
#' @export
sim_dist <- function(n_evaluations,
                     n_selections,
                     n_plots = 20,
                     n_simulations = 50000) {

  # Allow for n_selections to be a single integer
  stopifnot((n_evaluations == length(n_selections)) | (length(n_selections) == 1))
  n_selections <- rep(n_selections, length = n_evaluations)

  # Define weights for plots in lineups
  plot_weights <- matrix(stats::runif(n_plots * n_simulations), ncol = n_plots)

  result <- matrix(ncol = n_evaluations, nrow = n_simulations)
  for (i in 1:n_evaluations) {
    result[,i] <- apply(plot_weights,
                        MARGIN = 1,
                        function(plot_weights_single_lineup) {
                          1 %in% sample(1:n_plots,
                                        size = n_selections[i],
                                        prob = plot_weights_single_lineup)
                        })
  }

  # Aggregate all subjects
  result <- factor(rowSums(result), levels = 0:n_evaluations)

  # Average the count to get the approximated distribution
  return(c(table(result)/n_simulations))
}


# calc_p_value ------------------------------------------------------------

#' Calculate p-value for a visual test
#'
#' This function calculate the p-value for a visual test. The null distribution
#' is simulated by using function [sim_dist()]. The p-value is the probability
#' of the number of detections greater or equal to the observed value.
#'
#' It is encouraged to provide a cache environment to boost up the performance
#' when this function needs to be reused. The cache environment will remember
#' the result corresponding to the combinations of `n_evaluations`
#' and `n_simulations`.
#'
#' @param n_detections Integer. Observed value of number of detections.
#' @param n_evaluations,n_selections,n_plots,n_simulations Arguments passed to [sim_dist()].
#' @param cache_env Environment. A provided environment for caching.
#' @param seed Integer. [set.seed()] will be run at the beginning of the
#' function if `seed` is provided.
#' @return A numeric value representing the p-value.
#'
#' @examples
#' calc_p_value(2, 3, c(1,1,2))
#' calc_p_value(1, 1, c(1))
#'
#' @export
calc_p_value <- function(n_detections,
                         n_evaluations,
                         n_selections,
                         n_plots = 20,
                         n_simulations = 50000,
                         cache_env = NULL,
                         seed = NULL) {

  if (n_detections > n_evaluations) stop("Number of detected plots greater than number of evaluations.")


  if(!is.null(seed)) {
    set.seed(seed)
  }

  target_dist <- NULL
  uid <- paste0(n_simulations, "s",
                paste0(n_selections, collapse = "_"))

  # Load result from cache
  if (is.environment(cache_env)) target_dist <- cache_env$cache_dist[[uid]]

  if (is.null(target_dist)) {
    target_dist <- sim_dist(n_evaluations,
                            n_selections,
                            n_plots,
                            n_simulations)

    # Cache result
    if (is.environment(cache_env)) cache_env$cache_dist[[uid]] <- target_dist
  }

  if (n_detections == 0) {
    return(1)
  } else {
    return(1 - unname(cumsum(target_dist)[n_detections]))
  }
}
