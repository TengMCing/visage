MODEL_SPEC <- function(model = new.env(parent = parent.frame()), ...) {

  # The MODEL_SPEC class constructor

  # Pass CMD check
  rbinom <- rlnorm <- rnorm <- runif <- self <- NULL

  # Load pkg fns
  define_pkg_fn("stats", rbinom, rlnorm, rnorm, runif)

  # Crate a new environment and store parameters
  model$parameters <- list(...)
  model$self <- model

  gen_x_ <- function(n) {

    # The default method of generating x.
    #
    # Available arguments:
    #
    # x_dist: the distribution of x, one of "uniform", "normal", "lognormal", "neglognormal",
    #   "uniform": (-1, 1),
    #   "normal": normal(x_mu, x_sigma),
    #   "lognormal": lognormal(x_mu, x_sigma^2)/3 - 1,
    #   "neglognormal": -lognormal(x_mu, x_sigma^2)/3 + 1,
    #   "discrete_uniform": n possible values from [-1, 1],
    #   "binomial": binomial(n, 0.5)/n*2 - 1.
    #
    # x_mu: mean of lognormal or normal.
    # x_sigma: sigma of lognormal or normal.
    # x_n: n of binomial or discrete_uniform.
    # z_discrete: TRUE or FALSE. Whether or not to introduce an additional discrete z. Otherwise, z ~ U(-1,1).
    #
    # z_discrete_dist: the distribution of discrete z, one of "discrete_uniform" and "binomial",
    #   "discrete_uniform": n possible values from [-1, 1],
    #   "binomial": binomial(n, 0.5)/n*2 - 1.
    #
    # z_n: n of binomial or discrete_uniform.

    x_dist <- self$parameters$x_dist
    z_discrete <- self$parameters$z_discrete
    z_discrete_dist <- self$parameters$z_discrete_dist
    z_n <- self$parameters$z_n
    x_mu <- self$parameters$x_mu
    x_sigma <- self$parameters$x_sigma
    x_n <- self$parameters$x_n

    stand_dist <- function(n, func, ...) {
      result <- func(n, ...)
      result / max(result)
    }

    mylognormal <- function(n, meanlog, sdlog) {
      rlnorm(n, meanlog, sdlog)/3 - 1
    }

    myneglognormal <- function(n, meanlog, sdlog) {
      -mylognormal(n, meanlog, sdlog)
    }

    mybinom <- function(n, size, prob) {
      rbinom(n, size, prob)/size*2-1
    }

    x <- switch(as.character(x_dist),
                "uniform" = runif(n, -1, 1),
                "normal" = stand_dist(n, rnorm, mean = x_mu, sd = x_sigma),
                "lognormal" = stand_dist(n,
                                         mylognormal,
                                         meanlog = x_mu, sdlog = x_sigma),
                "neglognormal" = stand_dist(n,
                                            myneglognormal,
                                            meanlog = x_mu, sdlog = x_sigma),
                "discrete_uniform" = sample(runif(x_n, -1, 1), n, replace = TRUE),
                "binomial" = stand_dist(n, rbinom, size = x_n, prob = 0.5)
    )

    if (identical(z_discrete, TRUE)) {
      z <- switch(as.character(z_discrete_dist),
                  "discrete_uniform" = sample(runif(z_n, -1, 1), n, replace = TRUE),
                  "binomial" = stand_dist(n, rbinom, size = z_n, prob = 0.5)
      )

      x <- as.matrix(data.frame(x, z))
      colnames(x) <- NULL
    } else {
      z <- runif(n, -1, 1)

      x <- as.matrix(data.frame(x, z))
      colnames(x) <- NULL
    }

    return(x)
  }

  register_method(model,
                  gen_x = gen_x_)

  return(model)
}
