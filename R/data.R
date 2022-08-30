#' Results of a visual inference study on reading residual plots of misspecified linear regression model caused by missing Hermite polynomial terms
#'
#' A dataset containing the information of 160 subject and their responses to 588 linupes.
#' There are a total of 588 lineups, where lineup 577 - 588 are used as attention checks.
#' Every subject evaluates 18 different lineups and two randomly assigned attention checks.
#' Every lineup except those used as attention checks has been evaluated by five different subjects.
#' Every lineup consists of 20 residual plots with one actual residual plot and 19 null residual plots drawn with rotated residuals.
#'
#' To reproduce the models, use [poly_model()].
#'
#' For `x_dist = "uniform"`, define `x = rand_uniform(-1, 1)`.
#'
#' For `x_dist = "normal"`, define `x = {stand_dist <- function(x) {(x - min(x))/max(x - min(x)) * 2 - 1}; raw_x <- rand_normal(sigma = 0.3); closed_form(~stand_dist(raw_x))}`.
#'
#' For `x_dist = "lognormal"`, define `x = {stand_dist <- function(x) {(x - min(x))/max(x - min(x)) * 2 - 1}; raw_x <- rand_lognormal(sigma = 0.6); closed_form(~stand_dist(raw_x/3 - 1))}`.
#'
#' For `x_dist = "uniform_discrete"`, define `x = rand_uniform_d(k = 5, even = TRUE)`.
#'
#' For example, if `shape = 1`, `e_sigma = 1`, `include_z = TRUE` and `x_dist = "uniform"`,
#' then the model can be defined as `y = poly_model(shape = 1, sigma = 1, include_z = TRUE, x = rand_uniform(-1, 1))`.
#'
#' Note that the models will not produce exactly the same lineups as shown to
#' subjects due to randomness. Data stored in [get_polynomials_lineup()]
#' should be used instead.
#'
#' @format A tibble with 3200 rows and 30 variables:
#' \describe{
#'   \item{page}{The page number of the study website}
#'   \item{response_time}{Time spent on a page, in milliseconds (1 second = 1000 milliseconds)}
#'   \item{set}{The set number or the subject ID}
#'   \item{num}{The lineup number in a set}
#'   \item{selection}{Selections made by the subject. Multiple selections are allowed and seperated by `"_"`. `"0"` means the subject can't tell the difference between plots}
#'   \item{num_selection}{Number of selections made by the subject}
#'   \item{reason}{The reason for making the selections provided by the subject}
#'   \item{confidence}{Level of difference between the selected plots and others provided by the subject}
#'   \item{age_group}{Age group of the subject}
#'   \item{educatoin}{Educational background of the subject}
#'   \item{pronoun}{Preferred pronoun}
#'   \item{previous_experience}{Previous experience in any research that requires reading data graphs}
#'   \item{lineup_id}{Lineup ID}
#'   \item{type}{Type of the model}
#'   \item{formula}{The main formula of the model}
#'   \item{shape}{Shape of the Hermite polynomials, please check [POLY_MODEL$hermite]}
#'   \item{x_dist}{Distribution of the variable `x`}
#'   \item{include_z}{Whether to include variable `z` in the model}
#'   \item{e_dist}{Distribution of error term `e`}
#'   \item{e_sigma}{The standard deviation of the error term `e`}
#'   \item{name}{Name of the model}
#'   \item{k}{Number of residual plots in a lineup}
#'   \item{n}{Number of observations in a residual plot}
#'   \item{effect_size}{Effect size of the actual residual plot}
#'   \item{answer}{The answer of the lineup}
#'   \item{detect}{Whether the subject selects the actual residual plot}
#'   \item{conventional_p_value}{P-value of the conventional test (F-test) by comparing the null model (y ~ x) and the correct model (y ~ x + z)}
#'   \item{p_value}{P-value of the visual test calculated using [calc_p_value_multi()] with `n_sim = 1e+6`}
#'   \item{weigthed_detect}{If `detect == TRUE`, `weighted_detect = detect/num_selection`. Otherwise, `weighted_detect = 0`.}
#'   \item{prop_detect}{Poportion of detection of a lineup. For a lineup, `prop_detect = mean(weighted_detect)`.}
#' }
"polynomials"



#' Download the detailed information of lineups used in the polynomials study
#'
#' This function downloads and returns a list containing the information of 588
#' lineups including their metadata and observations. See also [polynomials].
#'
#' @return A list with 588 items, where each item contains a named list `metadata` and a data.frame `data`:
#'
#' **Metadata** contains metadata of the lineup:
#'
#' \describe{
#'   \item{type}{Type of the model}
#'   \item{formula}{The main formula of the model}
#'   \item{shape}{Shape of the Hermite polynomials, please check [POLY_MODEL$hermite]}
#'   \item{x_dist}{Distribution of variable `x`}
#'   \item{include_z}{Whether to include variable `z` in the model}
#'   \item{e_dist}{Distribution of error term `e`}
#'   \item{e_sigma}{The standard deviation of the error term `e`}
#'   \item{name}{Name of the model}
#'   \item{k}{Number of residual plots in a lineup}
#'   \item{n}{Number of observations in a residual plot}
#'   \item{effect_size}{Effect size of the actual residual plot}
#'   \item{answer}{The answer of the lineup}
#' }
#'
#' **Data** contains data for drawing 20 residual plots:
#'
#' \describe{
#'   \item{y}{Values of `y`}
#'   \item{raw_x}{Values of non-scaled `x`}
#'   \item{x}{Values of scaled `x` (between -1 to 1)}
#'   \item{raw_z}{Values of non-scaled `z`}
#'   \item{z}{Values of scaled `z` (between -1 to 1)}
#'   \item{e}{Values of the error term `e`}
#'   \item{.resid}{Value of residuals}
#'   \item{.fitted}{Fitted values}
#'   \item{test_name}{Name of the conventional test}
#'   \item{statistic}{Test statistics}
#'   \item{p_value}{P-value of the test}
#'   \item{k}{Position of the plot in a lineup}
#'   \item{null}{Whehter or not it is a null residual plot}
#' }
#'
#' @source \url{https://raw.githubusercontent.com/TengMCing/visage/master/data-raw/polynomials_lineup.rds}
#' @export
get_polynomials_lineup <- function() {

  # Save the Rds file in a temp file
  tmp <- tempfile()
  curl::curl_download("https://raw.githubusercontent.com/TengMCing/visage/master/data-raw/polynomials_lineup.rds",
                      destfile = tmp)
  polynomials_lineup <- readRDS(tmp)

  # Clean up
  unlink(tmp)

  polynomials_lineup
}

#' @export
get_heter_lineup <- function() {

  # Save the Rds file in a temp file
  tmp <- tempfile()
  curl::curl_download("https://raw.githubusercontent.com/TengMCing/visage/master/data-raw/heter_lineup.rds",
                      destfile = tmp)
  polynomials_lineup <- readRDS(tmp)

  # Clean up
  unlink(tmp)

  polynomials_lineup
}
