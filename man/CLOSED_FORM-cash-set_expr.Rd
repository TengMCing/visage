% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zzz_CLOSED_FORM.R
\name{CLOSED_FORM$set_expr}
\alias{CLOSED_FORM$set_expr}
\title{Set the closed form expression}
\arguments{
\item{expr}{Formula. Only the right hand side of the last \code{~} will be kept as
the final expression.}
}
\value{
Return the object itself.
}
\description{
This function updates the closed form expression. It will not
update the symbol values. For updating symbol values,
please check \link{CLOSED_FORM$set_sym}.
}
\examples{

# Constant variable
a <- 1

# Random uniform variable
z <- closed_form(~2 + a)

z$compute()

# Update the expression
z$set_expr(~2 - a)

z$compute()
}
