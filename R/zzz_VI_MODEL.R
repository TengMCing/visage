
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
#' New attributes: [VI_MODEL$prm], [VI_MODEL$prm_type], [VI_MODEL$..cache..],
#' [VI_MODEL$formula], [VI_MODEL$null_formula], VI_MODEL$alt_formula
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
#'                                prm_type = list(x = "r"),
#'                                formula = y~x+x^2)
#' test$prm
VI_MODEL$prm

#' List of parameter types
#'
#' @name VI_MODEL$prm_type
#'
#' @description A list, will be initialized after an instance is built. "r"
#' stands for random variable or closed form used in the expression of `y`,
#' "o" stands for others. This value only affects the string representation of
#' the object. If a variable is not part of the expression of `y`, then it
#' should be labelled as "o". If a variable is part of the expression of `y`,
#' but it is not a random variable or a closed form expression, then it should
#' be labelled as "o" as well.
#'
#' @examples
#'
#' # Instantiation
#' x <- RAND_UNIFORM$instantiation()
#' test <- VI_MODEL$instantiation(prm = list(x = x),
#'                                prm_type = list(x = "r"),
#'                                formula = y~x+x^2)
#' test$prm_type
VI_MODEL$prm_type

#' Cache list, containing the last fitted model, data frame and formula
#'
#' @name VI_MODEL$..cache..
#'
#' @description A list, will be used if `cache = TRUE` while calling the `fit`
#' method.
#'
#' @examples
#'
#' # Instantiation
#' x <- RAND_UNIFORM$instantiation()
#' test <- VI_MODEL$instantiation(prm = list(x = x),
#'                                prm_type = list(x = "r"),
#'                                formula = y~x+x^2,
#'                                null_formula = y~x,
#'                                alt_formula = y~x+I(x^2))
#'
#' # Turn on cache
#' test$fit(test$gen(10), cache = TRUE)
#'
#' test$..cache..
VI_MODEL$..cache..

#' Closed form expression of `y`
#'
#' @name VI_MODEL$formula
#'
#' @description A formula, will be passed to `CLOSED_FORM$instantiation` to
#' define a closed form expression for `y`.
#'
#' @examples
#'
#' # Instantiation
#' x <- RAND_UNIFORM$instantiation()
#' test <- VI_MODEL$instantiation(prm = list(x = x),
#'                                prm_type = list(x = "r"),
#'                                formula = y~x+x^2,
#'                                null_formula = y~x,
#'                                alt_formula = y~x+I(x^2))
#'
#' # A formula with environment equals to `test`
#' test$formula
#'
#' # `y` has the same expression
#' test$prm$y
VI_MODEL$formula

#' Formula for fitting the null model
#'
#' @name VI_MODEL$null_formula
#'
#' @description A formula, will be used to fit the null model.
#'
#' @examples
#'
#' # Instantiation
#' x <- RAND_UNIFORM$instantiation()
#' test <- VI_MODEL$instantiation(prm = list(x = x),
#'                                prm_type = list(x = "r"),
#'                                formula = y~x+x^2,
#'                                null_formula = y~x,
#'                                alt_formula = y~x+I(x^2))
#'
#' # A formula with environment equals to `test`
#' test$null_formula
#'
#' # By default, `fit` use the null formula
#' test$fit(test$gen(10))
#'
#' # F-test also needs to use the null model
#' test$test(test$gen(10))
VI_MODEL$null_formula

HIGHER_ORDER_MODEL <- class_HIGHER_ORDER_MODEL()
