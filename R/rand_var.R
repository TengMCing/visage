# RAND_VAR ----------------------------------------------------------------

class_RAND_VAR <- function(env = new.env(parent = parent.frame())) {

  # Pass CMD check
  self <- NULL

  new_class(BASE, env = env, class_name = "RAND_VAR")

  init_ <- function(dist = "uniform", prm = list()) {
    self$dist <- dist
    if (!is.list(prm)) stop("`prm` is not a list!")
    self$prm <- prm

    return(invisible(NULL))
  }

  gen_ <- function(n, ...) NA

  E_ <- function() NA

  Var_ <- function() NA

  str_ <- function() {
    if (self$..instantiated..) {
      init_string <- paste0("<", self$..type.., " object>")
    } else {
      init_string <- paste0("<", self$..type.., " class>")
    }

    con_string <- ""
    if (length(self$prm) > 0) con_string <- paste0(names(self$prm),
                                                   ": ",
                                                   round(unlist(self$prm), 3),
                                                   collapse = ", ")

    if (con_string == "") return(init_string)

    paste0(init_string, "\n [", con_string, "]")
  }

  register_method(env,
                  ..init.. = init_,
                  ..str.. = str_,
                  E = E_,
                  Var = Var_,
                  gen = gen_)

  return(env)
}


# RAND_UNIFORM ------------------------------------------------------------



class_RAND_UNIFORM <- function(env = new.env(parent = parent.frame())) {

  # Pass CMD check
  self <- NULL

  new_class(RAND_VAR, env = env, class_name = "RAND_UNIFORM")

  init_ <- function(a = 0, b = 1) {

    # Use the parent class `..init..` method
    use_method(self, RAND_VAR$..init..)(dist = "uniform",
                                        prm = list(a = a, b = b))

    return(invisible(NULL))
  }

  gen_ <- function(n, a = NULL, b = NULL) {

    if (is.null(a)) a <- self$prm$a
    if (is.null(b)) b <- self$prm$b

    if (length(a) == 1 & length(b) == 1) return(stats::runif(n, a, b))

    if (length(a) == 1) a <- rep(a, n)
    if (length(b) == 1) b <- rep(b, n)

    unlist(lapply(1:n, function(i) stats::runif(1, a[i], b[i])))
  }

  E_ <- function() (self$prm$a + self$prm$b)/2

  Var_ <- function() (self$prm$b - self$prm$a)^2/12

  register_method(env, ..init.. = init_, gen = gen_, E = E_, Var = Var_)

  return(env)
}

# RAND_NORMAL -------------------------------------------------------------



class_RAND_NORMAL <- function(env = new.env(parent = parent.frame())) {

  # Pass CMD check
  self <- NULL

  # Inherit from RAND_VAR class
  new_class(RAND_VAR, env = env, class_name = "RAND_NORMAL")

  init_ <- function(mu = 0, sigma = 1) {

    # Use the parent class `..init..` method
    use_method(self, RAND_VAR$..init..)(dist = "normal",
                                        prm = list(mu = mu, sigma = sigma))

    return(invisible(NULL))
  }

  gen_ <- function(n, mu = NULL, sigma = NULL) {

    if (is.null(mu)) mu <- self$prm$mu
    if (is.null(sigma)) sigma <- self$prm$sigma

    if (length(mu) == 1 & length(sigma) == 1) return(stats::rnorm(n, mu, sigma))

    if (length(mu) == 1) mu <- rep(mu, n)
    if (length(sigma) == 1) sigma <- rep(sigma, n)

    unlist(lapply(1:n, function(i) stats::rnorm(1, mu[i], sigma[i])))
  }

  E_ <- function() self$prm$mu

  Var_ <- function() self$prm$sigma^2

  register_method(env, ..init.. = init_, gen = gen_, E = E_, Var = Var_)

  return(env)
}

# RAND_LOGNORMAL ----------------------------------------------------------

class_RAND_LOGNORMAL <- function(env = new.env(parent = parent.frame())) {

  # Pass CMD check
  self <- NULL

  # Inherit from RAND_VAR class
  new_class(RAND_VAR, env = env, class_name = "RAND_LOGNORMAL")

  init_ <- function(mu = 0, sigma = 1) {

    # Use the parent class `..init..` method
    use_method(self, RAND_VAR$..init..)(dist = "lognormal",
                                        prm = list(mu = mu, sigma = sigma))

    return(invisible(NULL))
  }

  gen_ <- function(n, mu = NULL, sigma = NULL) {

    if (is.null(mu)) mu <- self$prm$mu
    if (is.null(sigma)) sigma <- self$prm$sigma

    if (length(mu) == 1 & length(sigma) == 1) return(stats::rlnorm(n, mu, sigma))

    if (length(mu) == 1) mu <- rep(mu, n)
    if (length(sigma) == 1) sigma <- rep(sigma, n)

    unlist(lapply(1:n, function(i) stats::rlnorm(1, mu[i], sigma[i])))
  }

  E_ <- function() exp(self$prm$mu + self$prm$sigma^2/2)

  Var_ <- function() (exp(self$prm$sigma^2) - 1) * exp(2 * self$prm$mu + self$prm$sigma^2)

  register_method(env, ..init.. = init_, gen = gen_, E = E_, Var = Var_)

  return(env)
}


# RAND_UNIFORM_D ----------------------------------------------------------

class_RAND_UNIFORM_D <- function(env = new.env(parent = parent.frame())) {

  # Pass CMD check
  self <- NULL

  # Inherit from RAND_VAR class
  new_class(RAND_VAR, env = env, class_name = "RAND_UNIFORM_D")

  init_ <- function(a = 0, b = 1, k = 5) {

    # Use the parent class `..init..` method
    use_method(self, RAND_VAR$..init..)(dist = "discrete uniform",
                                        prm = list(a = a, b = b, k = k))

    return(invisible(NULL))
  }

  gen_ <- function(n, a = NULL, b = NULL, k = NULL) {

    if (is.null(a)) a <- self$prm$a
    if (is.null(b)) b <- self$prm$b
    if (is.null(k)) k <- self$prm$k

    if (length(a) == 1 & length(b) == 1 & length(k) == 1) {
      cand <- stats::runif(k, a, b)
      sample(cand, n, replace = TRUE)
    }

    if (length(a) == 1) a <- rep(a, n)
    if (length(b) == 1) b <- rep(b, n)
    if (length(k) == 1) k <- rep(k, n)

    unlist(lapply(1:n, function(i) {
      cand <- stats::runif(k[i], a[i], b[i])
      sample(cand, 1, replace = TRUE)
      }))
  }

  E_ <- function() (self$prm$a + self$prm$b)/2

  Var_ <- function() (self$prm$b - self$prm$a)^2/12

  register_method(env, ..init.. = init_, gen = gen_, E = E_, Var = Var_)

  return(env)
}
