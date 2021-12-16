
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
#' @param num_evaluations Integer. Number of evaluations.
#' @param num_selections Integer. A vector of the number of selections.
#' @param num_plots Integer. Number of plots in the lineup.
#' @param num_simulations Integer. Number of simulations draws.
#' @return A named vector representing the probability mass function of the distribution.
#'
#' @examples
#' sim_dist(3, c(2,2,3))
#' sim_dist(1, c(1))
#'
#' @export
sim_dist <- function(num_evaluations,
                     num_selections,
                     num_plots = 20,
                     num_simulations = 50000) {

  # Allow for num_selections to be a single integer
  stopifnot((num_evaluations == length(num_selections)) | (length(num_selections) == 1))
  num_selections <- rep(num_selections, length = num_evaluations)

  # Define weights for plots in lineups
  plot_weights <- matrix(stats::runif(num_plots * num_simulations), ncol = num_plots)

  result <- matrix(ncol = num_evaluations, nrow = num_simulations)
  for (i in 1:num_evaluations) {
    result[,i] <- apply(plot_weights,
                        MARGIN = 1,
                        function(plot_weights_single_lineup) {
                          1 %in% sample(1:num_plots,
                                        size = num_selections[i],
                                        prob = plot_weights_single_lineup)
                        })
  }

  # Aggregate all subjects
  result <- factor(rowSums(result), levels = 0:num_evaluations)

  # Average the count to get the approximated distribution
  return(c(table(result)/num_simulations))
}


# calc_p_value ------------------------------------------------------------

#' Calculate p-value for a visual test
#'
#' This function calculate the p-value for a visual test. The null distribution
#' is simulated by using function [sim_dist()]. The p-value is the probability
#' of the number of detections greater or equal to the observed value.
#'
#' It is encouraged to provide a cache environment to boost up the performance
#' when you need to reuse this function. The cache environment will remember
#' the result corresponding to the combinations of `num_evaluations`
#' and `num_simulations`.
#'
#' @param num_detections Integer. Observed value of number of detections.
#' @param num_evaluations,num_selections,num_plots,num_simulations Arguments passed to [sim_dist()].
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
calc_p_value <- function(num_detections,
                         num_evaluations,
                         num_selections,
                         num_plots = 20,
                         num_simulations = 50000,
                         cache_env = NULL,
                         seed = NULL) {

  if (num_detections > num_evaluations) stop("Number of detected plots greater than number of evaluations.")


  if(!is.null(seed)) {
    set.seed(seed)
  }

  target_dist <- NULL
  uid <- paste0(num_simulations, "s",
                paste0(num_selections, collapse = "_"))

  # Load result from cache
  if (is.environment(cache_env)) target_dist <- cache_env$cache_dist[[uid]]

  if (is.null(target_dist)) {
    target_dist <- sim_dist(num_evaluations,
                            num_selections,
                            num_plots,
                            num_simulations)

    # Cache result
    if (is.environment(cache_env)) cache_env$cache_dist[[uid]] <- target_dist
  }

  if (num_detections == 0) {
    return(1)
  } else {
    return(1 - unname(cumsum(target_dist)[num_detections]))
  }
}
