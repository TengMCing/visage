RAND_VAR <- function(env = new.env(parent = parent.frame()), ...) {

  # pass CMD check
  self <- NULL

  # Inherit from BASE class
  env <- inherit(env, BASE, "RAND_VAR")

  len_ <- function() 1

  register_method(env,
                  len = len_)

  return(env)

  # The RAND_VAR class constructor

  env$name <- name
  env$self <- env
  env$parameters <- list()

  gen_ <- function(...) {

  }

  update_ <- function(...) self$parameters <- modifyList(self$parameters, list(...))

  methods_ <- function() names(self)[unlist(lapply(names(self), function(x) is.function(self[[x]])))]
  attrs_ <- function() {}

  register_method(env, gen = gen_, update = update_, methods = methods_)
  class(env) <- "RAND_VAR"
  return(env)
}

register_class_ctor(RAND_VAR, "RAND_VAR", parent = BASE)
