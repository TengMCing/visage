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
#' \eqn{W_i, i = 1, ..., N,} where \eqn{N} is the number of plots, and
#' \eqn{W_i} follows a uniform distribution independently. For each draw,
#' weights for a lineup will be simulated. Then, for each evaluation of a draw,
#' the function will sample same number of plots as the number of selection in
#' the evaluation using the simulated weights without replacement.
#' Finally, the distribution of the occurrences of plot 1 in a draw is the
#' approximated distribution of number of detections of a lineup.
#'
#' @param n_eval Integer. Number of evaluations.
#' @param n_sel Integer. A vector of the number of selections.
#' @param n_plot Integer. Number of plots in the lineup.
#' @param n_sim Integer. Number of simulations draws.
#' @return A named vector representing the probability mass function of the distribution.
#'
#' @examples
#' sim_dist(3, c(2,2,3))
#' sim_dist(1, c(1))
#'
#' @export
sim_dist <- function(n_eval,
                     n_sel,
                     n_plot = 20,
                     n_sim = 50000) {

  # Allow for n_sel to be a single integer
  stopifnot((n_eval == length(n_sel)) | (length(n_sel) == 1))
  n_sel <- rep(n_sel, length = n_eval)

  # Define weights for plots in lineups
  plot_weights <- matrix(stats::runif(n_plot * n_sim), ncol = n_plot)

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


# calc_p_value ------------------------------------------------------------

#' Calculate p-value for a visual test
#'
#' This function calculate the p-value for a visual test. The null distribution
#' is simulated by using function [sim_dist()]. The p-value is the probability
#' of the number of detections greater or equal to the observed value.
#'
#' It is encouraged to provide a cache environment to boost up the performance
#' when this function needs to be reused. The cache environment will remember
#' the result corresponding to the combinations of `n_eval`
#' and `n_sim`.
#'
#' @param n_detect Integer. Observed value of number of detections.
#' @param n_eval Integer. Number of evaluations.
#' @param n_sel Integer. A vector of the number of selections.
#' @param n_plot Integer. Number of plots in the lineup.
#' @param n_sim Integer. Number of simulations draws.
#' @param cache_env Environment. A provided environment for caching.
#' @return A numeric value representing the p-value.
#'
#' @examples
#' calc_p_value(2, 3, c(1,1,2))
#' calc_p_value(1, 1, c(1))
#'
#' @export
calc_p_value <- function(n_detect,
                         n_eval,
                         n_sel,
                         n_plot = 20,
                         n_sim = 50000,
                         cache_env = NULL) {

  if (n_detect > n_eval) stop("Number of detected plots greater than number of evaluations.")

  target_dist <- NULL
  n_sel <- sort(n_sel)
  uid <- paste0(n_sim, "s",
                paste0(n_sel, collapse = "_"))

  # Load result from cache
  if (is.environment(cache_env)) target_dist <- cache_env$cache_dist[[uid]]

  if (is.null(target_dist)) {
    target_dist <- sim_dist(n_eval,
                            n_sel,
                            n_plot,
                            n_sim)

    # Cache result
    if (is.environment(cache_env)) cache_env$cache_dist[[uid]] <- target_dist
  }

  if (n_detect == 0) {
    return(1)
  } else {
    return(1 - unname(cumsum(target_dist)[n_detect]))
  }
}



# calc_p_value_comb -------------------------------------------------------

#' Calculate p-value for all combinations of evaluations of a lineup.
#'
#' This function calculate p-value for all combinations of evaluations given
#' the desired number of evaluations. For a lineup that being evaluated
#' by \eqn{K} subjects, all combinations of \eqn{K} evaluations, taken
#' \eqn{X} at a time will be generated, where \eqn{X} is the desired number of
#' evaluations.
#'
#' It is encouraged to provide a cache environment to boost up the performance
#' when this function needs to be reused. The cache environment will remember
#' the result corresponding to the combinations of `n_eval`
#' and `n_sim`.
#'
#' @param detected Boolean. A vector of Boolean values indicating whether the
#' lineup has been detected by the subjects.
#' @param n_eval Integer. Desired number of evaluations.
#' @param n_sel Integer. A vector of the number of selections.
#' @param n_plot Integer. Number of plots in the lineup.
#' @param n_sim Integer. Number of simulation draws.
#' @param cache_env Environment. A provided environment for caching.
#' @return A vector of p-value with the combination matrix as the attribute.
#'
#' @examples
#' calc_p_value_comb(c(TRUE, FALSE, TRUE), 2, c(1, 1, 2))
#' calc_p_value_comb(c(TRUE, FALSE), 1, c(1, 1))
#'
#' @seealso [combn()]
#' @export
calc_p_value_comb <- function(detected,
                              n_eval,
                              n_sel,
                              n_plot = 20,
                              n_sim = 50000,
                              cache_env = NULL) {

  if (n_eval == 0) stop("Number of evaluations equals to zero. Can not compute combinations.")

  # Generate all combinations
  comb_mat <- utils::combn(length(detected), n_eval)
  result <- apply(comb_mat,
                  MARGIN = 2,
                  function(this_combo) {
                    calc_p_value(n_detect = sum(detected[this_combo]),
                                 n_eval = n_eval,
                                 n_sel = n_sel[this_combo],
                                 n_plot = n_plot,
                                 n_sim = n_sim,
                                 cache_env = cache_env)
                  })

  # Save combinations
  attr(result, "combinations") <- comb_mat
  return(result)
}

#' Calculate p-value for multiple lineups.
#'
#' This function calculates p-value for multiple lineups by using function
#' [calc_p_value()]. If `comb = TRUE`, then function [calc_p_value_comb()] will
#' be used instead.
#'
#' It is encouraged to provide a cache environment to boost up the performance.
#' The cache environment will remember the result corresponding to the
#' combinations of `n_eval` and `n_sim`. `replace_0` and `replace_full` can be
#' turned on to set evaluations with 0 or full selections to be false detection
#' with only one selection.
#'
#' @param dat Data.frame/Tibble. A data.frame or a tibble.
#' @param lineup_id Character. Column name of ids of lineup.
#' @param detected Character. Column name of whether the lineup is detected by
#' the subject.
#' @param n_sel Character. Column name of the number of selections.
#' @param comb Boolean. Whether to compute all the combinations.
#' See also [calc_p_value_comb()].
#' @param n_eval Integer. A vector of desired number of evaluations. Only used
#' when `comb = TRUE`.
#' @param replace_0 Boolean. Whether to give treatment to evaluations with
#' number of selections equal to 0.
#' @param replace_full Boolean. Whether to give treatment to evaluations with
#' number of selections equal to number of plots.
#' @param n_plot Integer. Number of plots.
#' @param n_sim Integer. Number of simulation draws.
#' @param cache_env Environment. A provided environment for caching.
#' @return If `comb = TRUE`, the function returns a tiible with columns
#' `lineup_id` and `p_value`, where `p_value` is a list of vectors. If
#' `comb = FALSE`, the `p_value` column is a vector.
#'
#' @examples
#' dat <- data.frame(lineup_id = c(1,1,2),
#'                   detected = c(TRUE, FALSE, TRUE),
#'                   n_sel = c(1,1,2))
#' calc_p_value_multi(dat, comb = TRUE, n_eval = 1:2)
#' calc_p_value_multi(dat)
#'
#' @export
calc_p_value_multi <- function(dat,
                               lineup_id = "lineup_id",
                               detected = "detected",
                               n_sel = "n_sel",
                               comb = FALSE,
                               n_eval = 1,
                               replace_0 = TRUE,
                               replace_full = TRUE,
                               n_plot = 20,
                               n_sim = 50000,
                               cache_env = NULL) {
  # Pass CMD check
  mutate <- `%>%` <- group_by <- summarise <- count <- filter <- bind_rows <- n <- NULL

  # Load pkg funcs, quote dplyr to pass CMD check
  define_pkg_fn("dplyr", mutate, `%>%`, group_by, summarise, count, filter, bind_rows)

  # Replace variable names
  dat <- tibble::tibble(lineup_id = dat[[lineup_id]],
                        detected = dat[[detected]],
                        n_sel = dat[[n_sel]])

  # Case: 0 selections
  if (replace_0) {
    dat <- mutate(dat, detected = ifelse(n_sel == 0, FALSE, detected)) %>%
      mutate(n_sel = ifelse(n_sel == 0, 1, n_sel))
  }

  # Case: full selections
  if (replace_full) {
    dat <- mutate(dat, detected = ifelse(n_sel == n_plot, FALSE, detected)) %>%
      mutate(n_sel = ifelse(n_sel == n_plot, 1, n_sel))
  }

  # Progress bar decorator
  calc_with_progress <- function(lineup_id, fn, ...) {
    pb$tick(token = list(what = lineup_id))
    return(do.call(fn, list(...)))
  }

  # Compute p-value for all combinations or not
  if (comb) {

    result <- NULL

    for (j in n_eval) {
      # Only use lineups with more than n_eval evaluations
      target_lineups <- dat %>%
        count(lineup_id) %>%
        filter(n > j - 1)

      # Init progress bar
      barstr <- paste0("[:spin] n_eval: ", j, " Lineup: :what [:bar] :current/:total (:percent) eta: :eta")
      pb <- progress::progress_bar$new(format = barstr,
                                       total = nrow(target_lineups),
                                       clear = FALSE,
                                       width = 60)

      tmp <- dat %>%
        filter(lineup_id %in% target_lineups$lineup_id) %>%
        group_by(lineup_id) %>%
        summarise(p_value = list(calc_with_progress(lineup_id = lineup_id,
                                                    fn = calc_p_value_comb,
                                                    detected = detected,
                                                    n_eval = j,
                                                    n_sel = n_sel,
                                                    n_plot = n_plot,
                                                    n_sim = n_sim,
                                                    cache_env = cache_env)),
                  n_eval = j,
                  total_eval = dplyr::n())

      if (is.null(result)) {
        result <- tmp
      } else {
        result <- bind_rows(result, tmp)
      }
    }

  } else {

    # Init progress bar
    barstr <- "[:spin] Lineup :what [:bar] :current/:total (:percent) eta: :eta"
    pb <- progress::progress_bar$new(format = barstr,
                                     total = length(unique(dat$lineup_id)),
                                     clear = FALSE,
                                     width = 60)

    result <- dat %>%
      group_by(lineup_id) %>%
      summarise(p_value = calc_with_progress(lineup_id = lineup_id,
                                             fn = calc_p_value,
                                             n_detect = sum(detected),
                                             n_eval = dplyr::n(),
                                             n_sel = n_sel,
                                             n_plot = n_plot,
                                             n_sim = n_sim,
                                             cache_env = cache_env))
  }

  return(result)
}
