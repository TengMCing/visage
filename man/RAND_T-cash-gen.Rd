% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zzz_RAND_VAR.R
\name{RAND_T$gen}
\alias{RAND_T$gen}
\title{Generate random values}
\arguments{
\item{n}{Integer. Number of observations.}

\item{mu}{Numeric. If it is provided and has length \code{n}, values will be used in
each realization.}

\item{tau}{Numeric. If it is provided and has length \code{n}, values will be used in
each realization.}

\item{df}{Integer. If it is provided and has length \code{n}, values will be used in
each realization.}
}
\value{
A vector of numeric values.
}
\description{
This function generates random values from the random variable.
\subsection{Usage}{

\if{html}{\out{<div class="sourceCode">}}\preformatted{RAND_T$gen(n, mu = NULL, tau = NULL, df = NULL)
}\if{html}{\out{</div>}}
}
}
\examples{

test <- rand_t(df = 12)
test$gen(10)

test$gen(3, mu = c(1,2,3), tau = c(2,3,4), df = 10)
}
\seealso{
\code{\link[stats:TDist]{stats::rt()}}, \code{\link[=sample]{sample()}}
}
