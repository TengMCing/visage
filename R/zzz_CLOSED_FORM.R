
# CLOSED_FORM -------------------------------------------------------------

CLOSED_FORM <- new.env()


#' CLOSED_FORM class environment
#'
#' @name CLOSED_FORM
#'
#' @description This is the base class of closed form expression, inherited from
#' [bandicoot::BASE]. It is an environment with S3 class `bandicoot_oop`.\cr\cr
#' For instantiation, any simple expressions can be provided,
#' as long as all the symbols exist in the current environment. Note that it
#' tries to evaluate **ALL the atomic symbols** in the expression during
#' initialization, and store the values in the object. Hence, calls like
#' `a$b` will also be decomposed as `$`, `a` and `b`, where `b` will be
#' interpreted as **a variable "b" exists in the current environment**. Therefore,
#' use `~a[["b"]]` instead of `~a$b`. And pre-define function like
#' `myfun = function() 1`, then use it in the expression `~myfun()`.
#'
#' @param expr Formula. Only the right hand side of the last `~` will be kept as
#' the final expression.
#' @param env Environment. The instance environment.
#' @param init_call Call. Contents of the `..init_call...` It is recommended to leave it
#' as default.
#' @return An instance environment.
#'
#' @details # Class information
#' ## Parent classes
#' * Direct:
#'    * [bandicoot::BASE]
#'
#' ## New attributes
#' * E:
#'    * [CLOSED_FORM$expr]
#' * S:
#'    * [CLOSED_FORM$sym]
#'    * [CLOSED_FORM$sym_name]
#'    * [CLOSED_FORM$sym_type]
#'
#' ## New methods
#' * A:
#'    * [CLOSED_FORM$as_dataframe]
#'    * [CLOSED_FORM$ast]
#' * C:
#'    * [CLOSED_FORM$compute]
#' * G:
#'    * [CLOSED_FORM$gen]
#' * I:
#'    * [CLOSED_FORM$..init..]
#' * L:
#'    * [CLOSED_FORM$..len..]
#' * S:
#'    * [CLOSED_FORM$..str..]
#'
#' @export
CLOSED_FORM

#' @describeIn CLOSED_FORM Class constructor, same as `CLOSED_FORM$instantiate()`.
#' @export
closed_form <- function(expr,
                        env = new.env(parent = parent.frame()),
                        init_call = sys.call()) {
  CLOSED_FORM$instantiate(expr = expr, env = env, init_call = init_call)
}

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
#' b <- rand_uniform()
#'
#' # Define a closed form expression
#' cf <- closed_form(~3 * (exp(a) + b))
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
#' d <- rand_normal()
#'
#' # Define a closed form expression with another closed form expression
#' cf2 <- closed_form(~cf + 3 * d)
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
#' b <- rand_uniform()
#'
#' # Define a closed form expression
#' cf <- closed_form(omit~omit~3 * (exp(a) + b))
#'
#' # Only the right hand side of the last `~` will be kept
#' cf$expr
#'
#' d <- rand_normal()
#'
#' # Define a closed form expression with another closed form expression
#' cf2 <- closed_form(~cf + 3 * d)
#'
#' cf2$expr
CLOSED_FORM$expr

#' Initialization method
#'
#' @name CLOSED_FORM$..init..
#'
#' @description This function will be called after an instance is built. User
#' input will be stored in the environment. Any simple expressions can be provided,
#' as long as all the symbols exist in the current environment. Note that this
#' function tries to evaluate **ALL the atomic symbols** in the expression during
#' initialization, and store the values in the object. Hence, calls like
#' `a$b` will also be decomposed as `$`, `a` and `b`, where `b` will be
#' interpreted as **a variable "b" exists in the current environment**. Therefore,
#' use `~a[["b"]]` instead of `~a$b`. And pre-define function like
#' `myfun = function() 1`, then use it in the expression `~myfun()`.
#' \cr
#' \cr
#' Random variables will be replaced by their returns of the `gen` method, which
#' are typically vectors.
#' \cr
#' \cr
#' Inner closed form expressions in a hierarchical closed form expression will
#' also be replaced by the returns of their `gen` method.
#'
#' ## Usage
#' ```
#' CLOSED_FORM$..init..(expr)
#' ```
#'
#' @param expr Formula. Only the right hand side of the last `~` will be kept as
#' the final expression.
#' @return Return the object itself.
#'
#' @examples
#'
#' # Constant variable
#' a <- 1
#'
#' # Random uniform variable
#' b <- rand_uniform()
#'
#' # Define a closed form expression
#' cf <- closed_form(~3 * (exp(a) + b))
#'
#' cf
#'
#' d <- rand_normal()
#'
#' # Define a closed form expression with another closed form expression
#' cf2 <- closed_form(~cf + 3 * d)
#'
#' cf2
CLOSED_FORM$..init..

#' Length of the object
#'
#' @name CLOSED_FORM$..len..
#'
#' @description This function recursively count symbols
#' stored in the closed form expression and nested closed form expression.
#'
#' ## Usage
#' ```
#' CLOSED_FORM$..len..()
#' ```
#'
#' @return An integer.
#'
#' @examples
#'
#' test <- closed_form(~1+1)
#' test$..len..()
CLOSED_FORM$..len..

#' String representation of the object
#'
#' @name CLOSED_FORM$..str..
#'
#' @description This function returns a string representation of the object.
#'
#' ## Usage
#' ```
#' CLOSED_FORM$..str..()
#' ```
#'
#' @return A string.
#'
#' @examples
#'
#' CLOSED_FORM$..str..()
#'
#' test <- closed_form(~1+1)
#' test$..str..()
CLOSED_FORM$..str..

#' Compute the expression without generating any random values
#'
#' @name CLOSED_FORM$compute
#'
#' @description This function computes a deterministic expression.
#'
#' ## Usage
#' ```
#' CLOSED_FORM$compute()
#' ```
#'
#' @return Numeric value.
#'
#' @examples
#'
#' test <- closed_form(~1+mean(c(1,2,3)))
#' test$compute()
CLOSED_FORM$compute

#' Generating random values from the expression
#'
#' @name CLOSED_FORM$gen
#'
#' @description This function generates random values from the expression.
#' Random values or closed form expression will share the same value as long
#' as they have the same name.
#'
#' ## Usage
#' ```
#' CLOSED_FORM$gen(n, rhs_val = FALSE, computed = NULL)
#' ```
#'
#' @param n Integer. Number of observations.
#' @param rhs_val Boolean. Whether or not to keep the right hand side values of the
#' expression.
#' @param computed List. If it is provided, random variables
#' or random closed form expression will use the values from the list, which
#' makes the expression potentially deterministic.
#' @return Numeric values.
#'
#' @examples
#'
#' # Constant variable
#' a <- 1
#'
#' # Random uniform variable
#' b <- rand_uniform()
#'
#' # Define a closed form expression
#' cf <- closed_form(~3 * (exp(a) + b))
#'
#' # Generate 5 values
#' cf$gen(5)
#'
#' # Generate 5 values, and keep RHS
#' cf$gen(5, rhs_val = TRUE)
#'
#' d <- rand_normal()
#'
#' # Define a closed form expression with another closed form expression
#' cf2 <- closed_form(~cf + 3 * d)
#'
#' # Generate 5 values
#' cf2$gen(5)
#'
#' # Generate 5 values, and keep RHS
#' cf2$gen(5, rhs_val = TRUE)
#'
#' # Define a closed form expression with two random variables of the same name
#' cf3 <- closed_form(~d + d)
#'
#' # Both `d` will share the same values
#' cf3$gen(5, rhs_val = TRUE)
#'
#' # Define a closed form expression with two closed form expressions of the same name
#' cf4 <- closed_form(~cf3 + cf3)
#'
#' # Both `cf3` will share the same values, both `d` will share the same values as well
#' cf4$gen(5, rhs_val = TRUE)
#'
#' # Define a closed form expression with two different closed form expressions,
#' # but contains same random variables
#' cf5 <- closed_form(~cf3 + cf4)
#'
#' # Both `d` in `cf3` and `cf4` will share the same value
#' cf5$gen(5, rhs_val = TRUE)
#'
#' # Control the value of `d`
#' cf5$gen(5, rhs_val = TRUE, computed = list(d = 1))
CLOSED_FORM$gen

#' Abstract syntax tree of the expression
#'
#' @name CLOSED_FORM$ast
#'
#' @description This function returns the abstract syntax tree of the expression.
#'
#' ## Usage
#' ```
#' CLOSED_FORM$ast(expr)
#' ```
#'
#' @param expr Language. Any quoted expression.
#' @return A list.
#'
#' @examples
#'
#' CLOSED_FORM$ast(quote(a + b))
#' CLOSED_FORM$ast(quote(exp(a) + b^2))
CLOSED_FORM$ast

#' Transforming list to data frame
#'
#' @name CLOSED_FORM$as_dataframe
#'
#' @description This function transforms the result generated by `gen` to a
#' data frame.
#'
#' ## Usage
#' ```
#' CLOSED_FORM$as_dataframe(dat, lhs = ".lhs")
#' ```
#'
#' @param dat Vector or List. Results generated by `gen` method.
#' @param lhs Character. The name of the expression.
#' @return A data frame.
#'
#' @examples
#'
#' # Constant variable
#' a <- 1
#'
#' # Random uniform variable
#' b <- rand_uniform()
#'
#' # Define a closed form expression
#' cf <- closed_form(~3 * (exp(a) + b))
#'
#' # Generate data
#' result1 <- cf$gen(10)
#'
#' cf$as_dataframe(result1)
#'
#' # Generate data and keep the RHS
#' result2 <- cf$gen(10, rhs_val = TRUE)
#'
#' # Specify the name
#' cf$as_dataframe(result2, lhs = "y")
CLOSED_FORM$as_dataframe

#' Set values for symbols
#'
#' @name CLOSED_FORM$set_sym
#'
#' @description This function stores the user inputs as the updated values of
#' symbols. List of symbol names and symbol types will be updated automatically.
#' There is no protection for values that should not be modified, which may lead
#' to error or loss of binding of some objects. Please use this function with
#' caution.
#'
#' ## Usage
#' ```
#' CLOSED_FORM$set_sym(sym_name, sym_value)
#' ```
#'
#' @param sym_name Vector or List. A sequence of character symbol names.
#' @param sym_value Vector or List. A sequence of symbol values.
#' @return Return the object itself.
#'
#' @examples
#'
#' # Constant variable
#' a <- 1
#'
#' # Random uniform variable
#' z <- closed_form(~2 + a)
#'
#' z$compute()
#'
#' # Update value for "a"
#' z$set_sym("a", 3)
#'
#' z$compute()
#'
#' # Update value for "+"
#' z$set_sym("+", list(function(a, b) a * b))
#'
#' z$compute()
CLOSED_FORM$set_sym

#' Set the closed form expression
#'
#' @name CLOSED_FORM$set_expr
#'
#' @description This function updates the closed form expression. It will not
#' update the symbol values. For updating symbol values,
#' please check [CLOSED_FORM$set_sym].
#'
#' ## Usage
#' ```
#' CLOSED_FORM$set_expr(expr)
#' ```
#'
#' @param expr Formula. Only the right hand side of the last `~` will be kept as
#' the final expression.
#' @return Return the object itself.
#'
#' @examples
#'
#' # Constant variable
#' a <- 1
#'
#' # Random uniform variable
#' z <- closed_form(~2 + a)
#'
#' z$compute()
#'
#' # Update the expression
#' z$set_expr(~2 - a)
#'
#' z$compute()
CLOSED_FORM$set_expr
