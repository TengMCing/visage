% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zzz_VI_MODEL.R
\name{HETER_MODEL$sample_effect_size}
\alias{HETER_MODEL$sample_effect_size}
\title{Compute the sample based effect size of the simulated data}
\arguments{
\item{dat}{Dataframe. The number of rows of the data frame will be used.}

\item{a}{Numeric. Default is \code{a = self$prm$a}.}

\item{b}{Numeric. Default is \code{b = self$prm$b}.}

\item{type}{Character. Type of the effect size. Default is \code{type = NULL}.}
}
\value{
A single numeric value.
}
\description{
This function computes the sample based effect size of the simulated data.
}
\examples{

mod <- heter_model(a = 0, b = 16)
mod$sample_effect_size(mod$gen(100))
}
