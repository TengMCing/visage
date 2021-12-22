RAND_VAR <- function(dist = "uniform", parameters = list(), ..., env = new.env(parent = parent.frame())) {

  # pass CMD check
  self <- NULL

  # Inherit from BASE class
  env <- inherit(env, BASE, "RAND_VAR", ...)
  env$depend <- NULL
  env$dist <- dist
  env$parameters <- parameters

  gen_ <- function() NULL

  register_method(env,
                  gen = gen_)

  return(env)
}

register_class_ctor(RAND_VAR, "RAND_VAR", parent = BASE)


RAND_VAR_UNIFORM <- function(a, b, ..., env = new.env(parent = parent.frame())) {

  # pass CMD check
  self <- NULL

  # Inherit from RAND_VAR class
  env <- inherit(env, RAND_VAR, "RAND_VAR_UNIFORM", dist = "uniform", ...)
  env$parameters$a <- a
  env$parameters$b <- b

  gen_ <- function(n) {
    runif(n, self$parameters$a, self$parameters$b)
  }

  register_method(env, gen = gen_)

  return(env)
}

register_class_ctor(RAND_VAR_UNIFORM, "RAND_VAR_UNIFORM", parent = RAND_VAR)
