# Create portals for class constructor

#' @name portal
#' @title Portals to class instantiation methods
#' @description These functions are the same as `class$instantiation`.
#' @param ... Arguments passed to `init` method.
#' @param env Environment. The instance environment.
NULL

#' @describeIn portal [BASE]
#' @export
base_ <- BASE$instantiation

#' @describeIn portal [RAND_VAR]
#' @export
rand_var <- RAND_VAR$instantiation

#' @describeIn portal [RAND_UNIFORM]
#' @export
rand_uniform <- RAND_UNIFORM$instantiation

#' @describeIn portal [RAND_UNIFORM_D]
#' @export
rand_uniform_d <- RAND_UNIFORM_D$instantiation

#' @describeIn portal [RAND_NORMAL]
#' @export
rand_normal <- RAND_NORMAL$instantiation

#' @describeIn portal [RAND_LOGNORMAL]
#' @export
rand_lognormal <- RAND_LOGNORMAL$instantiation

#' @describeIn portal [CLOSED_FORM]
#' @export
closed_form <- CLOSED_FORM$instantiation

#' @describeIn portal [VI_MODEL]
#' @export
vi_model <- VI_MODEL$instantiation

#' @describeIn portal [CUBIC_MODEL]
#' @export
cubic_model <- CUBIC_MODEL$instantiation

#' @describeIn portal [SIMPLE_CUBIC_MODEL]
#' @export
simple_cubic_model <- SIMPLE_CUBIC_MODEL$instantiation

#' @describeIn portal [QUARTIC_MODEL]
#' @export
quartic_model <- QUARTIC_MODEL$instantiation

#' @describeIn portal [HETER_MODEL]
#' @export
heter_model <- HETER_MODEL$instantiation
