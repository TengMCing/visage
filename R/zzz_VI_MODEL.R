
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
#' [VI_MODEL$formula], [VI_MODEL$null_formula], [VI_MODEL$alt_formula]
#' \cr
#' \cr
#' New methods: [VI_MODEL$..init..], [VI_MODEL$..str..], [VI_MODEL$set_formula],
#' [VI_MODEL$gen], [VI_MODEL$test], [VI_MODEL$fit], [VI_MODEL$plot_resid]
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
#' x <- rand_uniform()
#' e <- rand_normal()
#' test <- vi_model(prm = list(x = x, e = e),
#'                  prm_type = list(x = "r", e = "r"),
#'                  formula = y ~ 1 + x + x^2 + e)
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
#' x <- rand_uniform()
#' e <- rand_normal()
#' test <- vi_model(prm = list(x = x, e = e),
#'                  prm_type = list(x = "r", e = "r"),
#'                  formula = y ~ 1 + x + x^2 + e)
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
#' x <- rand_uniform()
#' e <- rand_normal()
#' test <- vi_model(prm = list(x = x, e = e),
#'                  prm_type = list(x = "r", e = "r"),
#'                  formula = y ~ 1 + x + x^2 + e,
#'                  null_formula = y ~ x,
#'                  alt_formula = y ~ x + I(x^2))
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
#' x <- rand_uniform()
#' e <- rand_normal()
#' test <- vi_model(prm = list(x = x, e = e),
#'                  prm_type = list(x = "r", e = "r"),
#'                  formula = y ~ 1 + x + x^2 + e,
#'                  null_formula = y ~ x,
#'                  alt_formula = y ~ x + I(x^2))
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
#' x <- rand_uniform()
#' e <- rand_normal()
#' test <- vi_model(prm = list(x = x, e = e),
#'                  prm_type = list(x = "r", e = "r"),
#'                  formula = y ~ 1 + x + x^2 + e,
#'                  null_formula = y ~ x,
#'                  alt_formula = y ~ x + I(x^2))
#'
#' # A formula with environment equals to `test`
#' test$null_formula
#'
#' # By default, `fit` use the null formula
#' test$fit(test$gen(10))
#'
#' # F-test also needs to use the null model
#' test$test(test$gen(1000))
VI_MODEL$null_formula

#' Formula for fitting the alternative model
#'
#' @name VI_MODEL$alt_formula
#'
#' @description A formula, will be used to fit the alternative model.
#'
#' @examples
#'
#' # Instantiation
#' x <- rand_uniform()
#' e <- rand_normal()
#' test <- vi_model(prm = list(x = x, e = e),
#'                  prm_type = list(x = "r", e = "r"),
#'                  formula = y ~ 1 + x + x^2 + e,
#'                  null_formula = y ~ x,
#'                  alt_formula = y ~ x + I(x^2))
#'
#' # A formula with environment equals to `test`
#' test$alt_formula
#'
#' # Use alternative formula in `fit`
#' test$fit(test$gen(10), test$alt_formula)
#'
#' # F-test also needs to use the alternative model
#' test$test(test$gen(1000))
VI_MODEL$alt_formula

#' Initialization method
#'
#' @name VI_MODEL$..init..
#'
#' @description This function will be called after an instance is built. User
#' input will be stored in the environment. The response variable of this model
#' is `y`.
#' @param prm Named List. A list of random variables or closed form expressions that
#' needs to be used while constructing `y`. Constant parameters could also be
#' supplied.
#' @param prm_type Named List. A list of characters denoting the type of the
#' parameters. "r" stands for random variable or closed form used in the
#' expression of `y`, "o" stands for others. This value only affects the
#' string representation of the object.
#' @param formula Formula. This will be passed to `CLOSED_FORM$instantiation` to
#' define a closed form expression for `y`. Default is `formula = self$formula`.
#' @param null_formula Formula. Formula for fitting the null model. Default is
#' `NULL`.
#' @param alt_formula Formula. Formula for fitting the alternative model.
#' Default is `NULL`.
#' @return No return value, called for side effects.
#'
#' @examples
#'
#' # Instantiation
#' x <- rand_uniform()
#' e <- rand_normal()
#' test <- vi_model(prm = list(x = x, e = e),
#'                  prm_type = list(x = "r", e = "r"),
#'                  formula = y ~ 1 + x + x^2 + e,
#'                  null_formula = y ~ x,
#'                  alt_formula = y ~ x + I(x^2))
#'
#' test
VI_MODEL$..init..

#' String representation of the object
#'
#' @name VI_MODEL$..str..
#'
#' @description This function returns a string representation of the object.
#' @return A string.
#'
#' @examples
#'
#' VI_MODEL$..str..()
#'
#' # Instantiation
#' x <- rand_uniform()
#' e <- rand_normal()
#' test <- vi_model(prm = list(x = x, e = e),
#'                  prm_type = list(x = "r", e = "r"),
#'                  formula = y ~ 1 + x + x^2 + e,
#'                  null_formula = y ~ x,
#'                  alt_formula = y ~ x + I(x^2))
#'
#' test$..str..()
VI_MODEL$..str..

#' Set formula for y, null model or alternative model
#'
#' @name VI_MODEL$set_formula
#'
#' @description This function store the formula in the environment, and lets
#' them points to the environment. It is not recommended to modify the formula
#' of `y` after the instance has been built, which may create confusion.
#' @param formula Formula. This will be passed to `CLOSED_FORM$instantiation` to
#' define a closed form expression for `y`. Default is `formula = NULL`.
#' @param null_formula Formula. Formula for fitting the null model. Default is
#' `NULL`.
#' @param alt_formula Formula. Formula for fitting the alternative model.
#' Default is `NULL`.
#' @return No return value, called for side effects.
#'
#' @examples
#'
#' # Instantiation
#' x <- rand_uniform()
#' e <- rand_normal()
#' test <- vi_model(prm = list(x = x, e = e),
#'                  prm_type = list(x = "r", e = "r"),
#'                  formula = y ~ 1 + x + x^2 + e,
#'                  null_formula = y ~ x,
#'                  alt_formula = y ~ x + I(x^2))
#'
#' test$set_formula(alt_formula = y ~ x + I(x^3))
#' test$alt_formula
VI_MODEL$set_formula

#' Generating random values from the expression of `y`
#'
#' @name VI_MODEL$gen
#'
#' @description This function generates random values from the expression of
#' `y`, and keeps all the right hand side information in a data frame.
#' @param n Integer. Number of observations.
#' @param fit_model Boolean. Whether or not to fit a null model to obtain the
#' fitted values and the residuals. Default is `FALSE`.
#' @param test Boolean. Whether or not to test the null model against the
#' alternative model to obtain the test statistic and the p-value.
#' Default is `FALSE`.
#' @return A data frame.
#'
#' @examples
#'
#' # Instantiation
#' x <- rand_uniform()
#' e <- rand_normal()
#' test <- vi_model(prm = list(x = x, e = e),
#'                  prm_type = list(x = "r", e = "r"),
#'                  formula = y ~ 1 + x + x^2 + e,
#'                  null_formula = y ~ x,
#'                  alt_formula = y ~ x + I(x^2))
#'
#' test$gen(10)
#'
#' test$gen(10, fit_model = TRUE)
#'
#' test$gen(10, test = TRUE)
VI_MODEL$gen

#' Test the null model against the alternative model
#'
#' @name VI_MODEL$test
#'
#' @description This function test the null model against the alternative model.
#' In this class, the test is a F-test computed using [stats::anova]. Derived
#' classes may have their own test procedures.
#' @param dat Data frame. A data frame containing all variables needed by the
#' `null_formula` and `alt_formula`.
#' @param null_formula Formula. Formula for fitting the null model. Default
#' is `null_formula = self$null_formula`.
#' @param alt_formula Formula. Formula for fitting the alternative model.
#' Default is `alt_formula = self$alt_formula`.
#' @return A list containing the test name, the test statistic and the p-value.
#'
#' @examples
#'
#' # Instantiation
#' x <- rand_uniform()
#' e <- rand_normal()
#' test <- vi_model(prm = list(x = x, e = e),
#'                  prm_type = list(x = "r", e = "r"),
#'                  formula = y ~ 1 + x + x^2 + e,
#'                  null_formula = y ~ x,
#'                  alt_formula = y ~ x + I(x^2))
#'
#'
#' dat <- test$gen(100000)
#' test$test(dat)
#'
#' test$test(dat, alt_formula = y ~ x + I(x^3))
VI_MODEL$test

#' Test the null model against the alternative model
#'
#' @name VI_MODEL$fit
#'
#' @description This function fit a linear model by using `stats::lm`.
#' @param dat Data frame. A data frame containing all variables needed by the
#' `formula`. Default is `dat = self$..cache..$dat`.
#' @param formula Formula. Formula for fitting the model. Default
#' is `formula = self$null_formula`.
#' @param cache Boolean. Whether or not to cache the model. Default is `FALSE`.
#' @param ... Arguments passed to `stats::lm`.
#' @return A fitted linear model.
#'
#' @examples
#'
#' # Instantiation
#' x <- rand_uniform()
#' e <- rand_normal()
#' test <- vi_model(prm = list(x = x, e = e),
#'                  prm_type = list(x = "r", e = "r"),
#'                  formula = y ~ 1 + x + x^2 + e,
#'                  null_formula = y ~ x,
#'                  alt_formula = y ~ x + I(x^2))
#'
#' test$fit(test$gen(100000))
#'
#' test$fit(test$gen(100000), formula = y ~ x + I(x^2))
VI_MODEL$fit

#' Plot the residuals vs fitted values plot
#'
#' @name VI_MODEL$plot_resid
#'
#' @description This function generate a residuals vs fitted values plot.
#' @param dat Data frame. A data frame containing `.resid` and `.fitted`.
#' @return A ggplot.
#'
#' @examples
#'
#' # Instantiation
#' x <- rand_uniform()
#' e <- rand_normal()
#' test <- vi_model(prm = list(x = x, e = e),
#'                  prm_type = list(x = "r", e = "r"),
#'                  formula = y ~ 1 + x + 10 * x^2 + e,
#'                  null_formula = y ~ x,
#'                  alt_formula = y ~ x + I(x^2))
#'
#' test$plot_resid(test$gen(1000, fit_model = TRUE))
VI_MODEL$plot_resid


HIGHER_ORDER_MODEL <- class_HIGHER_ORDER_MODEL()


# ..init.. = init_,
# ..str.. = str_,
# set_formula = set_formula_,
# gen = gen_,
# test = test_,
# fit = fit_,
# plot_resid = plot_resid_,
# plot = plot_,
# plot_lineup = plot_lineup_,
# rss = rss_,
# null_resid = null_resid_,
# gen_lineup = gen_lineup_
