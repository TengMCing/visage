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

  gen_ <- function(n) NA

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

  gen_ <- function(n) {
    stats::runif(n, self$prm$a, self$prm$b)
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

  gen_ <- function(n) {
    stats::rnorm(n, self$prm$mu, self$prm$sigma)
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

  gen_ <- function(n) {
    stats::rlnorm(n, self$prm$mu, self$prm$sigma)
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

  gen_ <- function(n) {
    cand <- stats::runif(self$prm$k, self$prm$a, self$prm$b)
    sample(cand, n, replace = TRUE)
  }

  E_ <- function() (self$prm$a + self$prm$b)/2

  Var_ <- function() (self$prm$b - self$prm$a)^2/12

  register_method(env, ..init.. = init_, gen = gen_, E = E_, Var = Var_)

  return(env)
}

#
# # RAND_VAR ----------------------------------------------------------------
#
# RAND_VAR <- function(dist = "uniform", prm = list(), ..., env = new.env(parent = parent.frame())) {
#
#   # pass CMD check
#   self <- NULL
#
#   # Inherit from BASE class
#   env <- inherit(env, BASE, "RAND_VAR", ...)
#   env$dist <- dist
#   env$prm <- prm
#
#   gen_ <- function() NULL
#
#   E_ <- function() NA
#
#   Var_ <- function() NA
#
#   string_ <- function() {
#     init_string <- paste0("<", self$class[1], " object>")
#     con_string <- paste0(names(self$prm), ": ", round(unlist(self$prm), 3), collapse = ", ")
#     paste0(init_string, "\n [", con_string, "]")
#   }
#
#   register_method(env,
#                   gen = gen_,
#                   string = string_,
#                   E = E_,
#                   Var = Var_)
#
#   return(env)
# }
#
# register_class_ctor(RAND_VAR, "RAND_VAR", parent = BASE)
#
#
#
# # RAND_UNIFORM --------------------------------------------------------
#
# RAND_UNIFORM <- function(a, b, ..., env = new.env(parent = parent.frame())) {
#
#   # Pass CMD check
#   self <- NULL
#
#   # Inherit from RAND_VAR class
#   env <- inherit(env, RAND_VAR, "RAND_UNIFORM", dist = "uniform", ...)
#   env$prm$a <- a
#   env$prm$b <- b
#
#   if (env$prm$a > env$prm$b) stop("a > b!")
#
#   gen_ <- function(n) {
#     stats::runif(n, self$prm$a, self$prm$b)
#   }
#
#   E_ <- function() (self$prm$a + self$prm$b)/2
#
#   Var_ <- function() (self$prm$b - self$prm$a)^2/12
#
#   register_method(env, gen = gen_, E = E_, Var = Var_)
#
#   return(env)
# }
#
# register_class_ctor(RAND_UNIFORM, "RAND_UNIFORM", parent = RAND_VAR)
#
#
#
# register_class_ctor(RAND_VAR_NORMAL, "RAND_VAR_NORMAL", parent = RAND_VAR)
#
#
# # RAND_VAR_LOGNORMAL ------------------------------------------------------
#
# RAND_VAR_LOGNORMAL <- function(mean, sd, ..., env = new.env(parent = parent.frame())) {
#
#   # Pass CMD check
#   self <- NULL
#
#   # Inherit from RAND_VAR class
#   env <- inherit(env, RAND_VAR, "RAND_VAR_LOGNORMAL", dist = "lognormal", ...)
#   env$prm$mean <- mean
#   env$prm$sd <- sd
#

#
#   register_method(env, gen = gen_, E = E_, Var = Var_)
#
#   return(env)
# }
#
# register_class_ctor(RAND_VAR_LOGNORMAL, "RAND_VAR_LOGNORMAL", parent = RAND_VAR)
#
#
# # RAND_UNIFORM_D ------------------------------------------------------
#
# RAND_UNIFORM_D <- function(a, b, k, ..., env = new.env(parent = parent.frame())) {
#
#   # Pass CMD check
#   self <- NULL
#
#   # Inherit from RAND_VAR class
#   env <- inherit(env, RAND_VAR, "RAND_UNIFORM_D", dist = "uniform_discrete", ...)
#   env$prm$a <- a
#   env$prm$b <- b
#   env$prm$k <- k
#
#   gen_ <- function(n) {
#     candidates <- stats::runif(self$prm$k, self$prm$a, self$prm$b)
#     sample(candidates, n, replace = TRUE)
#   }
#
#   E_ <- function() (self$prm$a + self$prm$b)/2
#
#   Var_ <- function() (self$prm$b - self$prm$a)^2/12
#
#   register_method(env, gen = gen_, E = E_, Var = Var_)
#
#   return(env)
# }
#
# register_class_ctor(RAND_UNIFORM_D, "RAND_UNIFORM_D", parent = RAND_VAR)
