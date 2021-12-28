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

  E_ <- function() NA

  Var_ <- function() NA

  string_ <- function() {
    if (self$..instantiated..) {
      init_string <- paste0("<", self$..type.., " object>")
    } else {
      init_string <- paste0("<", self$..type.., " class>")
    }

    con_string <- paste0(names(self$prm), ": ", round(unlist(self$prm), 3), collapse = ", ")

    paste0(init_string, "\n [", con_string, "]")
  }

  register_method(env,
                  ..init.. = init_,
                  E = E_,
                  Var = Var_)

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
# # RAND_VAR_UNIFORM --------------------------------------------------------
#
# RAND_VAR_UNIFORM <- function(a, b, ..., env = new.env(parent = parent.frame())) {
#
#   # Pass CMD check
#   self <- NULL
#
#   # Inherit from RAND_VAR class
#   env <- inherit(env, RAND_VAR, "RAND_VAR_UNIFORM", dist = "uniform", ...)
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
# register_class_ctor(RAND_VAR_UNIFORM, "RAND_VAR_UNIFORM", parent = RAND_VAR)
#
#
# # RAND_VAR_NORMAL ---------------------------------------------------------
#
# RAND_VAR_NORMAL <- function(mean, sd, ..., env = new.env(parent = parent.frame())) {
#
#   # Pass CMD check
#   self <- NULL
#
#   # Inherit from RAND_VAR class
#   env <- inherit(env, RAND_VAR, "RAND_VAR_NORMAL", dist = "normal", ...)
#   env$prm$mean <- mean
#   env$prm$sd <- sd
#
#   gen_ <- function(n) {
#     stats::rnorm(n, self$prm$mean, self$prm$sd)
#   }
#
#   E_ <- function() self$prm$mean
#
#   Var_ <- function() self$prm$sd^2
#
#   register_method(env, gen = gen_, E = E_, Var = Var_)
#
#   return(env)
# }
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
#   gen_ <- function(n) {
#     stats::rlnorm(n, self$prm$mean, self$prm$sd)
#   }
#
#   E_ <- function() exp(self$prm$mean + self$prm$sd^2/2)
#
#   Var_ <- function() (exp(self$prm$sd^2) - 1) * exp(2 * self$prm$mean + self$prm$sd^2)
#
#   register_method(env, gen = gen_, E = E_, Var = Var_)
#
#   return(env)
# }
#
# register_class_ctor(RAND_VAR_LOGNORMAL, "RAND_VAR_LOGNORMAL", parent = RAND_VAR)
#
#
# # RAND_VAR_UNIFORM_D ------------------------------------------------------
#
# RAND_VAR_UNIFORM_D <- function(a, b, k, ..., env = new.env(parent = parent.frame())) {
#
#   # Pass CMD check
#   self <- NULL
#
#   # Inherit from RAND_VAR class
#   env <- inherit(env, RAND_VAR, "RAND_VAR_UNIFORM_D", dist = "uniform_discrete", ...)
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
# register_class_ctor(RAND_VAR_UNIFORM_D, "RAND_VAR_UNIFORM_D", parent = RAND_VAR)
