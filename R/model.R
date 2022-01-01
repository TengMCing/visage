# MODEL -------------------------------------------------------------------

class_MODEL <- function(env = new.env(parent = parent.frame())) {

  # Pass CMD check
  self <- NULL

  new_class(BASE, env = env, class_name = "MODEL")

  init_ <- function(name = "") {}

  sim <- function() {}

  fit <- function() {}

  plot <- function() {}

  return(env)
}



# LINEAR_MODEL ------------------------------------------------------------

class_LINEAR_MODEL <- function(env = new.env(parent = parent.frame())) {

  # Pass CMD check
  self <- NULL

  new_class(BASE, env = env, class_name = "LINEAR_MODEL")

  fit_ <- function(f, dat, ...) {

    # Correct the environment of the formula
    environment(f) <- environment()

    # Use `substitute` to let `lm` correctly record the call
    eval(substitute(stats::lm(formula = f, data = dat, ...)))
  }

  register_method(env, fit = fit_)

  return(env)
}
