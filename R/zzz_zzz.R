# Create portals for class constructor

#' @name portal
#' @title Portals to class instantiate methods
#' @description These functions are the same as `class$instantiate`.
#' @param ... Arguments passed to `init` method.
#' @param env Environment. The instance environment.
#' @param init_call Call. Contents of the `..init_call..`. It is recommended to
#' leave it as default. Default is `init_call = sys.call()`.
NULL

#' @describeIn portal [RAND_VAR]
#' @export
rand_var <- function(..., env = new.env(parent = parent.frame()), init_call = sys.call()) {
  RAND_VAR$instantiate(..., env = env, init_call = init_call)
}

#' @describeIn portal [RAND_UNIFORM]
#' @export
rand_uniform <- function(..., env = new.env(parent = parent.frame()), init_call = sys.call()) {
  RAND_UNIFORM$instantiate(..., env = env, init_call = init_call)
}

#' @describeIn portal [RAND_UNIFORM_D]
#' @export
rand_uniform_d <- function(..., env = new.env(parent = parent.frame()), init_call = sys.call()) {
  RAND_UNIFORM_D$instantiate(..., env = env, init_call = init_call)
}

#' @describeIn portal [RAND_NORMAL]
#' @export
rand_normal <- function(..., env = new.env(parent = parent.frame()), init_call = sys.call()) {
  RAND_NORMAL$instantiate(..., env = env, init_call = init_call)
}

#' @describeIn portal [RAND_LOGNORMAL]
#' @export
rand_lognormal <- function(..., env = new.env(parent = parent.frame()), init_call = sys.call()) {
  RAND_LOGNORMAL$instantiate(..., env = env, init_call = init_call)
}

#' @describeIn portal [CLOSED_FORM]
#' @export
closed_form <- function(..., env = new.env(parent = parent.frame()), init_call = sys.call()) {
  CLOSED_FORM$instantiate(..., env = env, init_call = init_call)
}

#' @describeIn portal [VI_MODEL]
#' @export
vi_model <- function(..., env = new.env(parent = parent.frame()), init_call = sys.call()) {
  VI_MODEL$instantiate(..., env = env, init_call = init_call)
}

#' @describeIn portal [CUBIC_MODEL]
#' @export
cubic_model <- function(..., env = new.env(parent = parent.frame()), init_call = sys.call()) {
  CUBIC_MODEL$instantiate(..., env = env, init_call = init_call)
}

#' @describeIn portal [SIMPLE_CUBIC_MODEL]
#' @export
simple_cubic_model <- function(..., env = new.env(parent = parent.frame()), init_call = sys.call()) {
  SIMPLE_CUBIC_MODEL$instantiate(..., env = env, init_call = init_call)
}

#' @describeIn portal [QUARTIC_MODEL]
#' @export
quartic_model <- function(..., env = new.env(parent = parent.frame()), init_call = sys.call()) {
  QUARTIC_MODEL$instantiate(..., env = env, init_call = init_call)
}

#' @describeIn portal [POLY_MODEL]
#' @export
poly_model <- function(..., env = new.env(parent = parent.frame()), init_call = sys.call()) {
  POLY_MODEL$instantiate(..., env = env, init_call = init_call)
}

#' @describeIn portal [HETER_MODEL]
#' @export
heter_model <- function(..., env = new.env(parent = parent.frame()), init_call = sys.call()) {
  HETER_MODEL$instantiate(..., env = env, init_call = init_call)
}

.onLoad <- function(libname, pkgname) {

  # Classes are empty environments defined by new.env()
  # Build them at load-time to ensure dependencies (e.g. bandicoot::BASE) are latest

  class_RAND_VAR(RAND_VAR)
  class_RAND_UNIFORM(RAND_UNIFORM)
  class_RAND_UNIFORM_D(RAND_UNIFORM_D)
  class_RAND_NORMAL(RAND_NORMAL)
  class_RAND_LOGNORMAL(RAND_LOGNORMAL)

  class_CLOSED_FORM(CLOSED_FORM)

  class_VI_MODEL(VI_MODEL)
  class_CUBIC_MODEL(CUBIC_MODEL)
  class_SIMPLE_CUBIC_MODEL(SIMPLE_CUBIC_MODEL)
  class_QUARTIC_MODEL(QUARTIC_MODEL)
  class_POLY_MODEL(POLY_MODEL)
  class_HETER_MODEL(HETER_MODEL)
}
