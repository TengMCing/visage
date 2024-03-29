% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\name{get_polynomials_lineup}
\alias{get_polynomials_lineup}
\title{Download the detailed information of lineups used in the polynomials study}
\source{
\url{https://raw.githubusercontent.com/TengMCing/visage/master/data-raw/polynomials_lineup.rds}
}
\usage{
get_polynomials_lineup()
}
\value{
A list with 588 items, where each item contains a named list \code{metadata} and a data.frame \code{data}:

\strong{Metadata} contains metadata of the lineup:

\describe{
\item{type}{Type of the model}
\item{formula}{The main formula of the model}
\item{shape}{Shape of the Hermite polynomials, please check \link{POLY_MODEL$hermite}}
\item{x_dist}{Distribution of variable \code{x}}
\item{include_z}{Whether to include variable \code{z} in the model}
\item{e_dist}{Distribution of error term \code{e}}
\item{e_sigma}{The standard deviation of the error term \code{e}}
\item{name}{Name of the model}
\item{k}{Number of residual plots in a lineup}
\item{n}{Number of observations in a residual plot}
\item{effect_size}{Effect size of the actual residual plot}
\item{answer}{The answer of the lineup}
}

\strong{Data} contains data for drawing 20 residual plots:

\describe{
\item{y}{Values of \code{y}}
\item{raw_x}{Values of non-scaled \code{x}}
\item{x}{Values of scaled \code{x} (between -1 to 1)}
\item{raw_z}{Values of non-scaled \code{z}}
\item{z}{Values of scaled \code{z} (between -1 to 1)}
\item{e}{Values of the error term \code{e}}
\item{.resid}{Value of residuals}
\item{.fitted}{Fitted values}
\item{test_name}{Name of the conventional test}
\item{statistic}{Test statistics}
\item{p_value}{P-value of the test}
\item{k}{Position of the plot in a lineup}
\item{null}{Whehter or not it is a null residual plot}
}
}
\description{
This function downloads and returns a list containing the information of 588
lineups including their metadata and observations. See also \link{polynomials}.
}
