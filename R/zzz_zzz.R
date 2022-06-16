# Create portals for class constructor

#' @name portal
#' @title Portals to class instantiate methods
#' @description These functions are the same as `class$instantiate`.
#' @param ... Arguments passed to `init` method.
#' @param env Environment. The instance environment.
NULL

#' @describeIn portal [VI_MODEL]
#' @export
vi_model <- VI_MODEL$instantiate

#' @describeIn portal [CUBIC_MODEL]
#' @export
cubic_model <- CUBIC_MODEL$instantiate

#' @describeIn portal [SIMPLE_CUBIC_MODEL]
#' @export
simple_cubic_model <- SIMPLE_CUBIC_MODEL$instantiate

#' @describeIn portal [QUARTIC_MODEL]
#' @export
quartic_model <- QUARTIC_MODEL$instantiate

#' @describeIn portal [POLY_MODEL]
#' @export
poly_model <- POLY_MODEL$instantiate

#' @describeIn portal [HETER_MODEL]
#' @export
heter_model <- HETER_MODEL$instantiate
