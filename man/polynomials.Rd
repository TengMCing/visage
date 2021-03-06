% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{polynomials}
\alias{polynomials}
\title{Results of a visual inference study on reading residual plots of misspecified linear regression model caused by missing Hermite polynomial terms}
\format{
A tibble with 3200 rows and 28 variables:
\describe{
\item{page}{The page number of the study website}
\item{response_time}{Time spent on a page, in milliseconds (1 second = 1000 milliseconds)}
\item{set}{The set number or the subject ID}
\item{num}{The lineup number in a set}
\item{selection}{Selections made by the subject. Multiple selections are allowed and seperated by \code{"_"}. \code{"0"} means the subject can't tell the difference between plots}
\item{num_selection}{Number of selections made by the subject}
\item{reason}{The reason for making the selections provided by the subject}
\item{confidence}{Level of difference between the selected plots and others provided by the subject}
\item{age_group}{Age group of the subject}
\item{educatoin}{Educational background of the subject}
\item{pronoun}{Preferred pronoun}
\item{previous_experience}{Previous experience in any research that requires reading data graphs}
\item{lineup_id}{Lineup ID}
\item{type}{Type of the model}
\item{formula}{The main formula of the model}
\item{shape}{Shape of the Hermite polynomials, please check \link{POLY_MODEL$hermite}}
\item{x_dist}{Distribution of the variable \code{x}}
\item{include_z}{Whether to include variable \code{z} in the model}
\item{e_dist}{Distribution of error term \code{e}}
\item{e_sigma}{The standard deviation of the error term \code{e}}
\item{name}{Name of the model}
\item{k}{Number of residual plots in a lineup}
\item{n}{Number of observations in a residual plot}
\item{effect_size}{Effect size of the actual residual plot}
\item{answer}{The answer of the lineup}
\item{detect}{Whether the subject selects the actual residual plot}
\item{conventional_p_value}{P-value of the conventional test (F-test) by comparing the null model (y ~ x) and the correct model (y ~ x + z)}
\item{p_value}{P-value of the visual test calculated using \code{\link[=calc_p_value_multi]{calc_p_value_multi()}} with \code{n_sim = 1e+6}}
}
}
\usage{
polynomials
}
\description{
A dataset containing the information of 160 subject and their responses to 588 linupes.
There are a total of 588 lineups, where lineup 577 - 588 are used as attention checks.
Every subject evaluates 18 different lineups and two randomly assigned attention checks.
Every lineup except those used as attention checks has been evaluated by five different subjects.
Every lineup consists of 20 residual plots with one actual residual plot and 19 null residual plots drawn with rotated residuals.
}
\details{
To reproduce the models, use \code{\link[=poly_model]{poly_model()}}.

For \code{x_dist = "uniform"}, define \code{x = rand_uniform(-1, 1)}.

For \code{x_dist = "normal"}, define \code{x = {stand_dist <- function(x) {(x - min(x))/max(x - min(x)) * 2 - 1}; raw_x <- rand_normal(sigma = 0.3); closed_form(~stand_dist(raw_x))}}.

For \code{x_dist = "lognormal"}, define \code{x = {stand_dist <- function(x) {(x - min(x))/max(x - min(x)) * 2 - 1}; raw_x <- rand_lognormal(sigma = 0.6); closed_form(~stand_dist(raw_x/3 - 1))}}.

For \code{x_dist = "uniform_discrete"}, define \code{x = rand_uniform_d(k = 5, even = TRUE)}.

For example, if \code{shape = 1}, \code{e_sigma = 1}, \code{include_z = TRUE} and \code{x_dist = "uniform"},
then the model can be defined as \code{y = poly_model(shape = 1, sigma = 1, include_z = TRUE, x = rand_uniform(-1, 1))}.

Note that the models will not produce exactly the same lineups as shown to
subjects due to randomness. Data stored in \code{\link[=get_polynomials_lineup]{get_polynomials_lineup()}}
should be used instead.
}
\keyword{datasets}
