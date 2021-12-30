
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
#' New attributes: [CLOSED_FORM$sym], [CLOSED_FORM$sym_name],
#' [CLOSED_FORM$sym_type], [CLOSED_FORM$expr]
#' \cr
#' \cr
#' New methods: CLOSED_FORM$..init.., CLOSED_FORM$..str..,
#' CLOSED_FORM$compute, CLOSED_FORM$gen, CLOSED_FORM$gen_rhs,
#' CLOSED_FORM$ast
#' @export
CLOSED_FORM <- class_CLOSED_FORM()

#' List of symbols in the abstract syntax tree of the expression
#'
#' @name CLOSED_FORM$sym
#'
#' @description A list, will be initialized after an instance is built.
#'
#' @examples
#'
#' # Constant variable
#' a <- 1
#'
#' # Random uniform variable
#' b <- RAND_UNIFORM$instantiation()
#'
#' # Define a closed form expression
#' cf <- CLOSED_FORM$instantiation(~3 * (exp(a) + b))
#'
#' cf
#'
#' # Get the list of symbols
#' cf$sym
#'
#' # Get the list of symbol names
#' cf$sym_name
#'
#' # Get the list of symbol types
#' cf$sym_type
#'
#' d <- RAND_NORMAL$instantiation()
#'
#' # Define a closed form expression with another closed form expression
#' cf2 <- CLOSED_FORM$instantiation(~cf + 3 * d)
#'
#' cf2
#'
#' # Get the list of symbols, constants are not counted as symbols
#' cf2$sym
#'
#' # Get the list of symbol names
#' cf2$sym_name
#'
#' # Get the list of symbol types
#' cf$sym_type
CLOSED_FORM$sym

#' List of symbol names in the abstract syntax tree of the expression
#'
#' @name CLOSED_FORM$sym_name
#'
#' @description A list, will be initialized after an instance is built.
#'
#' @seealso Examples and usages: [CLOSED_FORM$sym]
CLOSED_FORM$sym_name

#' List of symbol types in the abstract syntax tree of the expression
#'
#' @name CLOSED_FORM$sym_type
#'
#' @description A list, will be initialized after an instance is built.
#'
#' @seealso Examples and usages: [CLOSED_FORM$sym]
CLOSED_FORM$sym_type

#' Expression extracted from the provided formula
#'
#' @name CLOSED_FORM$expr
#'
#' @description A language object.
#'
#' @examples
#'
#' # Constant variable
#' a <- 1
#'
#' # Random uniform variable
#' b <- RAND_UNIFORM$instantiation()
#'
#' # Define a closed form expression
#' cf <- CLOSED_FORM$instantiation(omit~omit~3 * (exp(a) + b))
#'
#' # Only the right hand side of the last `~` will be kept
#' cf$expr
#'
#' d <- RAND_NORMAL$instantiation()
#'
#' # Define a closed form expression with another closed form expression
#' cf2 <- CLOSED_FORM$instantiation(~cf + 3 * d)
#'
#' cf2$expr
CLOSED_FORM$expr
