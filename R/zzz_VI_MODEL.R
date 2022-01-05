
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
#' New attributes: VI_MODEL$prm, VI_MODEL$prm_type, VI_MODEL$..cache..,
#' VI_MODEL$formula, VI_MODEL$null_formula, VI_MODEL$alt_formula
#' \cr
#' \cr
#' New methods: CLOSED_FORM$..init.., CLOSED_FORM$..str..,
#' CLOSED_FORM$compute, CLOSED_FORM$gen, CLOSED_FORM$ast
#' @export
VI_MODEL <- class_VI_MODEL()




HIGHER_ORDER_MODEL <- class_HIGHER_ORDER_MODEL()
