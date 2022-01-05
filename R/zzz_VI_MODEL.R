
# VI_MODEL ----------------------------------------------------------------

#' VI_MODEL class environment
#'
#' @name VI_MODEL
#'
#' @description This is the base class of visual inference linear model,
#' inherited from [BASE].
#' @format An environment with S3 class `oop`.
#' @seealso Parent class: [BASE]
#' \cr
#' \cr
#' New attributes: [VI_MODEL$prm], VI_MODEL$prm_type, VI_MODEL$..cache..,
#' VI_MODEL$formula, VI_MODEL$null_formula, VI_MODEL$alt_formula
#' \cr
#' \cr
#' New methods: VI_MODEL$..init.., VI_MODEL$..str..,
#' VI_MODEL$compute, CLOSED_FORM$gen, CLOSED_FORM$ast
#' @export
VI_MODEL <- class_VI_MODEL()

#' List of parameters
#'
#' @name VI_MODEL$prm
#'
#' @description A list, will be initialized after an instance is built.
#'
#' @examples
#'
#' # Instantiation
#' x <- RAND_UNIFORM$instantiation()
#' test <- VI_MODEL$instantiation(prm = list(x = x),
#'                                prm_type = list(x = "rand_var or closed_form"),
#'                                formula = y~x+x^2)
#' test$prm
VI_MODEL$prm



HIGHER_ORDER_MODEL <- class_HIGHER_ORDER_MODEL()
