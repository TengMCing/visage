# eval_p_value --------------------------------------------------------

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
#' eval_p_value(c(0.05, 0.0501), 0.05, 1e-3)
#' eval_p_value(c(0.05, 0.0501), 0.05, 1e-5)
#'
#' @export
eval_p_value <- function(p_value, significance_level = 0.05, tol = 1e-6) {
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
#' \eqn{W_i, i = 1, ..., M,} where \eqn{M} is the number of plots, and
#' \eqn{W_i} follows a attractiveness distribution independently. For each draw,
#' weights for a lineup will be simulated. Then, for each evaluation of a draw,
#' the function will sample same number of plots as the number of selection in
#' the evaluation using the simulated weights without replacement.
#' Finally, the distribution of the occurrences of plot 1 in a draw is the
#' approximated distribution of number of detections of a lineup.
#'
#' There are two attractiveness distribution available,
#' one is uniform distribution, another is Dirichlet distribution. Uniform
#' distribution ensures the marginal distribution of the probability of every
#' plot being selected is uniform.
#' When \eqn{\alpha = 1},
#' Dirichlet distribution ensures the probability of every plot being selected
#' is evenly distributed in a standard \eqn{M - 1} simplex.
#'
#' @param n_sel Integer. A vector of the number of selections.
#' @param n_plot Integer. Number of plots in the lineup.
#' @param n_sim Integer. Number of simulations draws. More simulation draws
#' will result in more accurate results.
#' @param dist Character. Name of the distribution used for the attractiveness simulation.
#' One of "uniform" and "dirichlet".
#' @param alpha Numeric. A single parameter value used by the Dirichlet distribution.
#' @return A named vector representing the probability mass function of the distribution.
#'
#' @examples
#'
#' # The first person select 2 plots, the second one select 2 plots and
#' # the third person select 3 plots.
#' sim_dist(c(2, 2, 3))
#'
#' # There is only one observer and it selects one plot from the lineup.
#' sim_dist(1)
#'
#' # Dirichlet distribution will be better if you want to take into account the
#' # possible dependencies between responses due to the same lineup is
#' # presented to multiple observers.
#' # There is no need to use this distribution if there is only one observer.
#' sim_dist(c(2, 2, 3), dist = "dirichlet")
#'
#' @export
sim_dist <- function(n_sel,
                     n_plot = 20,
                     n_sim = 50000,
                     dist = "uniform",
                     alpha = 1) {

  n_eval <- length(n_sel)

  if (!dist %in% c("uniform", "dirichlet")) stop("Argument `dist` needs to be either 'uniform' or 'dirichlet'!")

  # Define weights for plots in lineups
  if (dist == "uniform") plot_weights <- matrix(stats::runif(n_plot * n_sim), ncol = n_plot)
  if (dist == "dirichlet") plot_weights <- matrix(stats::rgamma(n_plot * n_sim, alpha, 1), ncol = n_plot)

  result <- matrix(ncol = n_eval, nrow = n_sim)
  for (i in 1:n_eval) {
    result[,i] <- apply(plot_weights,
                        MARGIN = 1,
                        function(plot_weights_single_lineup) {
                          1 %in% sample(1:n_plot,
                                        size = n_sel[i],
                                        prob = plot_weights_single_lineup)
                        })
  }

  # Aggregate all subjects
  result <- factor(rowSums(result), levels = 0:n_eval)

  # Average the count to get the approximated distribution
  return(c(table(result)/n_sim))
}


# exact_dist --------------------------------------------------------------

#' Calculate the exact distribution of number of detections of a lineup
#'
#' This function calculate the exact distribution of number of detections of a
#' lineup assuming every subject selects exactly one plot.
#'
#' For a given lineup, plots are assumed to have weights
#' \eqn{W_i, i = 1, ..., M,} where \eqn{M} is the number of plots, and
#' \eqn{W_i} follows a attractiveness distribution independently. \cr\cr
#' There are two attractiveness distribution available,
#' one is uniform distribution, another is Dirichlet distribution. Uniform
#' distribution ensures the marginal distribution of the probability of every
#' plot being selected is uniform.
#' When \eqn{\alpha = 1},
#' Dirichlet distribution ensures the probability of every plot being selected
#' is evenly distributed in a standard \eqn{M - 1} simplex.
#'
#' @param n_eval Integer. Number of evaluations.
#' @param n_plot Integer. Number of plots in the lineup.
#' @param dist Character. Name of the distribution used for the attractiveness model.
#' One of "uniform" and "dirichlet".
#' @param alpha Numeric. A single parameter value used by the Dirichlet distribution.
#' @return A named vector representing the probability mass function of the distribution.
#'
#' @examples
#' sim_dist(c(2,2,3))
#' sim_dist(1)
#'
#' @export
exact_dist <- function(n_eval, n_plot = 20, dist = "dirichlet", alpha = 1) {
  if (dist == "uniform") stop("Unimplemented featurs!")
  if (dist == "dirichlet") {
    n_detect <- 0:n_eval
    result <- choose(n_eval, n_detect) * beta(n_detect + alpha, n_eval - n_detect + (n_plot - 1) * alpha) / beta(alpha, (n_plot - 1) * alpha)
    names(result) <- n_detect
    return(result)
  }
}

# calc_p_value_dirichlet_linear_approx ------------------------------------

# calc_p_value_dirichlet_linear_approx <- function(detect, n_eval, n_sel, alpha, n_plot = 20) {
#
#   # Allow for n_sel to be a single integer
#   stopifnot((n_eval == length(n_sel)) | (length(n_sel) == 1))
#   n_sel <- rep(n_sel, length = n_eval)
#
#   if (sum(detect) == 0) return(1)
#   target_dist <- exact_dist_dirichlet(n_eval, alpha, n_plot)
#   if (all(n_sel == 1)) return(1 - unname(cumsum(target_dist)[sum(detect)]))
#
#   weighted_total_detect <- sum(ifelse(n_sel == 0, 1/n_plot, detect/n_sel))
#   floor_total <- floor(weighted_total_detect)
#   ceil_total <- ceiling(weighted_total_detect)
#
#   unname(sum(target_dist[(ceil_total:n_eval) + 1]) + (ceil_total - weighted_total_detect) * target_dist[floor_total + 1])
# }


# calc_p_value ------------------------------------------------------------

#' Calculate p-value for a visual test
#'
#' This function calculate the p-value for a visual test.
#'
#' If `method == "simulate"`,
#' the null distribution is simulated by using function [sim_dist()] and the
#' p-value is the probability of the number of detections greater or equal to
#' the observed value. \cr\cr
#' If `method == "linear_approx"`, the null
#' distribution is calculated using function [exact_dist()] and the p-value
#' is the probability of the detections greater or equal to the weighted
#' observed value. The weighted observed detections is calculated by incrementing
#' 1/n_sel for a detection, and 1/n_plot for zero selections. Since the null
#' distribution is discrete, and weighted observed detections is a real value,
#' linear approximation is used for the decimal part.\cr\cr
#' It is encouraged to provide a cache environment to boost up the performance
#' when this function needs to be reused and `method == "simulate"`.
#' The cache environment will remember
#' the result corresponding to the combinations of `n_eval`
#' and `n_sim`.\cr\cr
#' There are two attractiveness distribution available,
#' one is uniform distribution, another is Dirichlet distribution. Uniform
#' distribution ensures the marginal distribution of the probability of every
#' plot being selected is uniform.
#' When \eqn{\alpha = 1},
#' Dirichlet distribution ensures the probability of every plot being selected
#' is evenly distributed in a standard \eqn{M - 1} simplex.
#'
#' @param detect Integer. A vector of boolean values indicating whether the
#' subject detect the actual data plot.
#' @param n_sel Integer. A vector of the number of selections.
#' @param n_plot Integer. Number of plots in the lineup.
#' @param n_sim Integer. Number of simulations draws.
#' @param dist Character. Name of the distribution used for the attractiveness simulation.
#' One of "uniform" and "dirichlet".
#' @param alpha Numeric. A single parameter value used by the Dirichlet distribution.
#' @param cache_env Environment. A provided environment for caching.
#' @param method Character. Method of p-value calculation. "linear_approx" for linear
#' approximation and "simulate" for simulation.
#' @param replace_0 Boolean. Whether to treat evaluations with number of
#' selections equal to 0 as selecting all the plots.
#' @return A numeric value representing the p-value.
#'
#' @examples
#' calc_p_value(c(1, 1, 0), c(1, 1, 2), alpha = 0.5)
#' calc_p_value(1, 1, 1)
#'
#' @export
calc_p_value <- function(detect,
                         n_sel,
                         n_plot = 20,
                         n_sim = 50000,
                         dist = "dirichlet",
                         alpha = 1,
                         cache_env = NULL,
                         method = "linear_approx",
                         replace_0 = TRUE) {

  if (replace_0) {
    detect[n_sel == 0] <- 1
    n_sel[n_sel == 0] <- n_plot
  }

  n_detect <- sum(detect)
  n_eval <- length(detect)
  if (n_detect > n_eval) stop("Number of detected plots greater than number of evaluations.")

  if (method == "linear_approx" && dist == "dirichlet") {
      if (sum(detect) == 0) return(1)
      target_dist <- exact_dist(n_eval, n_plot, dist = "dirichlet", alpha = alpha)
      if (all(n_sel == 1)) return(1 - unname(cumsum(target_dist)[sum(detect)]))

      weighted_total_detect <- sum(ifelse(n_sel == 0, 1/n_plot, detect/n_sel))
      floor_total <- floor(weighted_total_detect)
      ceil_total <- ceiling(weighted_total_detect)

      return(unname(sum(target_dist[(ceil_total:n_eval) + 1]) + (ceil_total - weighted_total_detect) * target_dist[floor_total + 1]))
  }

  if (method == "linear_approx" && dist == "uniform") {
    stop("Unimplemented feature")
  }

  if (method == "simulate") {
    target_dist <- NULL
    n_sel <- sort(n_sel)
    uid <- paste0(dist, "_", alpha, "_", n_sim, "s",
                  paste0(n_sel, collapse = "_"))

    # Load result from cache
    if (is.environment(cache_env)) target_dist <- cache_env$cache_dist[[uid]]

    if (is.null(target_dist)) {
      target_dist <- sim_dist(n_sel,
                              n_plot,
                              n_sim,
                              dist,
                              alpha)

      # Cache result
      if (is.environment(cache_env)) cache_env$cache_dist[[uid]] <- target_dist
    }

    if (n_detect == 0) {
      return(1)
    } else {
      return(1 - unname(cumsum(target_dist)[n_detect]))
    }
  }

}



# calc_p_value_comb -------------------------------------------------------

# Calculate p-value for all combinations of evaluations of a lineup.
#
# This function calculate p-value for all combinations of evaluations given
# the desired number of evaluations. For a lineup that being evaluated
# by \eqn{K} subjects, all combinations of \eqn{K} evaluations, taken
# \eqn{X} at a time will be generated, where \eqn{X} is the desired number of
# evaluations.
#
# It is encouraged to provide a cache environment to boost up the performance
# when this function needs to be reused. The cache environment will remember
# the result corresponding to the combinations of `n_eval`
# and `n_sim`.
#
# There are two attractiveness distribution available,
# one is uniform distribution, another is Dirichlet distribution. Uniform
# distribution ensures the marginal distribution of the probability of every
# plot being selected is uniform.
# When \eqn{\alpha = 1},
# Dirichlet distribution ensures the probability of every plot being selected
# is evenly distributed in a standard \eqn{M - 1} simplex.
#
# @param detect Boolean. A vector of Boolean values indicating whether the
# lineup has been detected by the subjects.
# @param n_eval Integer. Desired number of evaluations.
# @param n_sel Integer. A vector of the number of selections.
# @param n_plot Integer. Number of plots in the lineup.
# @param n_sim Integer. Number of simulation draws.
# @param cache_env Environment. A provided environment for caching.
# @param dist Character. Name of the distribution used for the attractiveness simulation.
# One of "uniform" and "dirichlet".
# @param alpha Numeric. A single parameter value used by the Dirichlet distribution.
# @return A vector of p-value with the combination matrix as the attribute.
#
# @examples
# calc_p_value_comb(c(TRUE, FALSE, TRUE), 2, c(1, 1, 2))
# calc_p_value_comb(c(TRUE, FALSE), 1, c(1, 1))
#
# @seealso [combn()]
# @export
# calc_p_value_comb <- function(detect,
#                               n_eval,
#                               n_sel,
#                               n_plot = 20,
#                               n_sim = 50000,
#                               cache_env = NULL,
#                               dist = "uniform",
#                               alpha = 1) {
#
#   if (n_eval == 0) stop("Number of evaluations equals to zero. Can not compute combinations.")
#
#   # Generate all combinations
#   comb_mat <- utils::combn(length(detect), n_eval)
#   result <- apply(comb_mat,
#                   MARGIN = 2,
#                   function(this_combo) {
#                     calc_p_value(n_detect = sum(detect[this_combo]),
#                                  n_eval = n_eval,
#                                  n_sel = n_sel[this_combo],
#                                  n_plot = n_plot,
#                                  n_sim = n_sim,
#                                  cache_env = cache_env,
#                                  dist = dist,
#                                  alpha = alpha)
#                   })
#
#   # Save combinations
#   attr(result, "combinations") <- comb_mat
#   return(result)
# }



# calc_p_value_multi ------------------------------------------------------

#' Calculate p-value for multiple lineups.
#'
#' This function calculates p-value for multiple lineups by using function
#' [calc_p_value()].
#'
#' It is encouraged to provide a cache environment to boost up the performance.
#' The cache environment will remember the result corresponding to the
#' combinations of `n_eval` and `n_sim`. `replace_0` can be
#' turned on to set evaluations with 0 selections to be false detection
#' with 20 selections.
#'
#' There are two attractiveness distribution available,
#' one is uniform distribution, another is Dirichlet distribution. Uniform
#' distribution ensures the marginal distribution of the probability of every
#' plot being selected is uniform.
#' When \eqn{\alpha = 1},
#' Dirichlet distribution ensures the probability of every plot being selected
#' is evenly distributed in a standard \eqn{M - 1} simplex.
#'
#' @param dat Data.frame/Tibble. A data.frame or a tibble.
#' @param lineup_id Column of ids of lineup.
#' @param detect Column of whether the lineup is detected by the subject.
#' @param n_sel Column of the number of selections.
#' @param alpha Column of the parameter values used by the Dirichlet distribution.
#' @param n_plot Integer. Number of plots.
#' @param n_sim Integer. Number of simulation draws.
#' @param dist Character. Name of the distribution used for the attractiveness simulation.
#' One of "uniform" and "dirichlet".
#' @param cache_env Environment. A provided environment for caching.
#' @param method Character. Method of p-value calculation. "linear_approx" for linear
#' approximation and "simulate" for simulation.
#' @param replace_0 Boolean. Whether to treat evaluations with number of
#' selections equal to 0 as selecting all the plots.
#' @return A tibble with one column for lineup id and one column for the p-value.
#'
#' @examples
#' dat <- data.frame(unique_lineup_id = c(1,1,2),
#'                   detect_or_not = c(TRUE, FALSE, TRUE),
#'                   num_selection = c(1,1,2))
#' calc_p_value_multi(dat, unique_lineup_id, detect_or_not, num_selection)
#'
#' @export
calc_p_value_multi <- function(dat,
                               lineup_id,
                               detect,
                               n_sel,
                               alpha,
                               n_plot = 20,
                               n_sim = 50000,
                               dist = "dirichlet",
                               cache_env = NULL,
                               method = "linear_approx",
                               replace_0 = TRUE) {
  # Pass CMD check
  mutate <- `%>%` <- group_by <- summarise <- count <- filter <- bind_rows <- n <- NULL

  # Load pkg funcs, quote dplyr to pass CMD check
  bandicoot::define_pkg_fn("dplyr", mutate, `%>%`, group_by, summarise, count, filter, bind_rows)

  lineup_id_val <- eval(substitute(with(dat, lineup_id)), envir = parent.frame())
  detect_val <- eval(substitute(with(dat, detect)), envir = parent.frame())
  n_sel_val <- eval(substitute(with(dat, n_sel)), envir = parent.frame())

  if (missing(alpha))
    alpha_val <- 1
  else
    alpha_val <- eval(substitute(with(dat, alpha)), envir = parent.frame())

  # Replace variable names
  result <- tibble::tibble(lineup_id = lineup_id_val,
                           detect = detect_val,
                           n_sel = n_sel_val,
                           alpha = alpha_val)

  # Progress bar decorator
  calc_with_progress <- function(lineup_id, fn, ...) {
    pb$tick(token = list(what = lineup_id))
    return(do.call(fn, list(...)))
  }

  # Init progress bar
  barstr <- "[:spin] Lineup :what [:bar] :current/:total (:percent) eta: :eta"
  pb <- progress::progress_bar$new(format = barstr,
                                   total = length(unique(result$lineup_id)),
                                   clear = FALSE,
                                   width = 60)

  result <- result %>%
    group_by(lineup_id) %>%
    summarise(p_value = calc_with_progress(lineup_id = lineup_id,
                                           fn = calc_p_value,
                                           detect = detect,
                                           n_sel = n_sel,
                                           n_plot = n_plot,
                                           n_sim = n_sim,
                                           dist = dist,
                                           alpha = alpha[1],
                                           cache_env = cache_env,
                                           method = method,
                                           replace_0 = replace_0))

  names(result) <- c(deparse(substitute(lineup_id)), "p_value")
  return(result)
}
