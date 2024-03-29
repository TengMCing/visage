% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zzz_RAND_VAR.R
\name{RAND_VAR$gen}
\alias{RAND_VAR$gen}
\title{Generate random values}
\arguments{
\item{n}{Integer. Number of observations.}

\item{...}{Ignored.}
}
\value{
NA. User needs to define their own \code{gen} method in derived class.
}
\description{
This function generates random values from the random variable.
\subsection{Usage}{

\if{html}{\out{<div class="sourceCode">}}\preformatted{RAND_VAR$gen(n, ...)
}\if{html}{\out{</div>}}
}
}
\examples{

test <- RAND_VAR$gen(10)
}
