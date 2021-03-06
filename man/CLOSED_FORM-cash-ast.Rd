% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zzz_CLOSED_FORM.R
\name{CLOSED_FORM$ast}
\alias{CLOSED_FORM$ast}
\title{Abstract syntax tree of the expression}
\arguments{
\item{expr}{Language. Any quoted expression.}
}
\value{
A list.
}
\description{
This function returns the abstract syntax tree of the exrpession
}
\examples{

CLOSED_FORM$ast(quote(a + b))
CLOSED_FORM$ast(quote(exp(a) + b^2))
}
