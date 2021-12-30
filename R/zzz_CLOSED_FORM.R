
# CLOSED_FORM -------------------------------------------------------------

#' CLOSED_FORM class environment
#'
#' @name CLOSED_FORM
#'
#' @description This is the base class of closed form expression, inherited from
#' [BASE].
#' @format An environment with S3 class `oop`.
#' @seealso Parent class: [BASE]
#' \cr
#' \cr
#' New attributes: CLOSED_FORM$sym, CLOSED_FORM$sym_name,
#' CLOSED_FORM$type, CLOSED_FORM$expr
#' \cr
#' \cr
#' New methods: CLOSED_FORM$..init.., CLOSED_FORM$..str..,
#' CLOSED_FORM$compute, CLOSED_FORM$gen, CLOSED_FORM$gen_rhs,
#' CLOSED_FORM$ast
#' @export
CLOSED_FORM <- class_CLOSED_FORM()


