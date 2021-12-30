
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
#' New methods: [CLOSED_FORM$..init..], [CLOSED_FORM$..str..],
#' [CLOSED_FORM$compute], [CLOSED_FORM$gen], [CLOSED_FORM$gen_rhs],
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

#' Initialization method
#'
#' @name CLOSED_FORM$..init..
#'
#' @description This function will be called after an instance is built. User
#' input will be stored in the environment. Any expressions can be provided,
#' as long as they can be computed in the current environment.
#' \cr
#' \cr
#' Random variables will be replaced with a vector of random values when is
#' called by the `gen` method.
#' \cr
#' \cr
#' Hierarchical closed form expression will also be replaced with a vector of
#' values when is called by the `gen` method.
#' @param epxr Formula. Only the right hand side of the last `~` will be kept as
#' the final expression.
#' @return No return value, called for side effects.
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
#' d <- RAND_NORMAL$instantiation()
#'
#' # Define a closed form expression with another closed form expression
#' cf2 <- CLOSED_FORM$instantiation(~cf + 3 * d)
#'
#' cf2
CLOSED_FORM$..init..

#' String representation of the object
#'
#' @name CLOSED_FORM$..str..
#'
#' @description This function returns a string representation of the object.
#' @return A string.
#'
#' @examples
#'
#' test <- CLOSED_FORM$instantiation(~1+1)
#' test$..str..()
CLOSED_FORM$..str..

#' Compute the expression without generating any random values
#'
#' @name CLOSED_FORM$compute
#'
#' @description This function computes a deterministic expression.
#' @return Numeric value.
#'
#' @examples
#'
#' test <- CLOSED_FORM$instantiation(~1+mean(c(1,2,3)))
#' test$compute()
CLOSED_FORM$compute

#' Generating random values from the expression
#'
#' @name CLOSED_FORM$gen
#'
#' @description This function generates random values from the expression.
#' Random values will be generated independtly as long as they don't share
#' the same symbol in the expression.
#' @param n Integer. Number of observations.
#' @param rhs Boolean. Whether or not to keep the right hand side values of the
#' expression.
#' @return Numeric values.
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
#' # Generate 5 values
#' cf$gen(5)
#'
#' # Generate 5 values, and keep RHS
#' cf$gen(5, rhs = TRUE)
#'
#' d <- RAND_NORMAL$instantiation()
#'
#' # Define a closed form expression with another closed form expression
#' cf2 <- CLOSED_FORM$instantiation(~cf + 3 * d)
#'
#' # Generate 5 values
#' cf2$gen(5)
#'
#' # Generate 5 values, and keep RHS
#' cf2$gen(5, rhs = TRUE)
#'
#' # Define a closed form expression with two random variables of the same name
#' cf3 <- CLOSED_FORM$instantiation(~d * d)
#'
#' # Both `d` will share the same values
#' cf3$gen(5, rhs = TRUE)
#'
#' # Define a closed form expression with two closed form expressions of the same name
#' cf4 <- CLOSED_FORM$instantiation(~cf3 * cf3)
#'
#' # Both `cf3` will share the same values, both `d` will share the same values as well
#' cf4$gen(5, rhs = TRUE)
#'
#' # Define a closed form expression with two different closed form expressions,
#' # but contains same random variables
#' cf5 <- CLOSED_FORM$instantiation(~cf3 * cf4)
#'
#' # Both `d` in `cf3` will share the same value, but is different with `d` in `cf4`
#' cf5$gen(5, rhs = TRUE)
CLOSED_FORM$gen

#' Generating random values from the expression with right hand side being kept
#'
#' @name CLOSED_FORM$gen_rhs
#'
#' @description This function generates random values from the expression
#' with right hand side being kept. Random values will be generated
#' independently as long as they don't share the same symbol in the expression.
#' @param n Integer. Number of observations.
#' @return Numeric values.
#'
#' @seealso [CLOSED_FORM$gen]
CLOSED_FORM$gen_rhs
