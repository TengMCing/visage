% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zzz_VI_MODEL.R
\name{POLY_MODEL$..init..}
\alias{POLY_MODEL$..init..}
\title{Initialization method}
\arguments{
\item{shape}{Integer. The shape of the orthogonal polynomial used in the model.
Note it should be a value between 1 to 4. Default is \code{shape = 1}.}

\item{sigma}{Positive numeric. Default is \code{sigma = 1}.}

\item{include_z}{Boolean. Whether or not to include \code{z} in the formula of \code{y}.
Default is \code{include_z = TRUE}.}

\item{x}{Random variable or closed form expression. Default is
\code{x = rand_uniform(-1, 1, env = new.env(parent = parent.env(self)))}.}

\item{e}{Random variable or closed form expression. Default is
\code{e = rand_normal(0, sigma, env = new.env(parent = parent.env(self)))}.}
}
\value{
Return the object itself.
}
\description{
This function will be called after an instance is built. User
input will be stored in the environment. The response variable of this model
is \code{y}. The formula of y is defined in \link{POLY_MODEL$formula}, the null
formula is defined in \link{POLY_MODEL$null_formula}, the alternative is
defined in \link{POLY_MODEL$alt_formula}. The formula for the raw
orthogonal polynomial term is defined in \link{POLY_MODEL$raw_z_formula}, and
the scaled orthogonal polynomial term is defined in \link{POLY_MODEL$z_formula}.
}
\examples{

# Instantiate
x <- rand_uniform()
e <- rand_normal(sigma = 0.5)

test <- poly_model(shape = 1, x = x, e = e)

test

# Generate data
test$gen(10)

# Generate lineup
test$gen_lineup(10, k = 3)

# Plot the lineup
test$plot_lineup(test$gen_lineup(100))

test <- poly_model(shape = 1, include_z = FALSE, x = x, e = e)
test$plot_lineup(test$gen_lineup(100))

test <- poly_model(shape = 2, x = x, e = e)
test$plot_lineup(test$gen_lineup(100))

test <- poly_model(shape = 3, x = x, e = e)
test$plot_lineup(test$gen_lineup(100))

test <- poly_model(shape = 4, x = x, e = e)
test$plot_lineup(test$gen_lineup(100))
}
