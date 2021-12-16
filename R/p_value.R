
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
  return(p_value < significance_level)
}


# sim_dist ----------------------------------------------------------------

#' Approximate the distribution of number of detections of a lineup with simulation
#'
#' This function approximate the distribution of number of detections of a
#' lineup for given number of evaluations, selections in each evaluation and
#' plots in a lineup.
#'
#' For a given lineup, plots are assumed to have weights
#' \deqn{W_i, i = 1, ..., num_plots}, where \eqn{w_i} follows a uniform
#' distribution independently. For each draw, weights for a lineup will be
#' simulated. Then, for each evaluation of a draw, the function will sample
#' same number of plots as the number of selection in the evaluation
#' using the simulated weights without replacement.
#' Finally, the distribution of the occurrences of plot 1 in a draw is the
#' approximated distribution of number of detections of a lineup.
#'
#' @param num_evaluations Integer. Number of evaluations.
#' @param num_selections Integer. A vector of the number of selections.
#' @param num_plots Integer. Number of plots in the lineup.
#' @param num_simulations Integer. Number of simulations draws.
#' @return A named vector repenting the probability mass function of the distribution
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

#' Calculate p-value for visual test
#'
#' This function
#' @noRd
calc_p_value <- function(num_detections,
                         num_evaluations,
                         num_selections,
                         num_plots = 20,
                         num_simulations = 50000,
                         cache_env = NULL,
                         seed = NULL) {
  if(!is.null(seed)) {
    set.seed(seed)
  }

}
