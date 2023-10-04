
# VI_MODEL ----------------------------------------------------------------

#' VI_MODEL class environment
#'
#' @name VI_MODEL
#'
#' @description This is the base class of visual inference linear model,
#' inherited from [bandicoot::BASE].
#' @format An environment with S3 class `bandicoot_oop`.
#' @seealso Parent class: [bandicoot::BASE]
#' \cr
#' \cr
#' New attributes: [VI_MODEL$prm], [VI_MODEL$prm_type],
#' [VI_MODEL$formula], [VI_MODEL$null_formula], [VI_MODEL$alt_formula]
#' \cr
#' \cr
#' New methods: [VI_MODEL$..init..], [VI_MODEL$..str..], [VI_MODEL$set_formula],
#' [VI_MODEL$gen], [VI_MODEL$test], [VI_MODEL$fit], [VI_MODEL$average_effect_size],
#' [VI_MODEL$plot_resid],
#' [VI_MODEL$plot_qq],
#' [VI_MODEL$plot], [VI_MODEL$plot_lineup], [VI_MODEL$rss],
#' [VI_MODEL$null_resid], [VI_MODEL$gen_lineup]
#' @export
VI_MODEL <- new.env()

#' List of parameters
#'
#' @name VI_MODEL$prm
#'
#' @description A list, will be initialized after an instance is built.
#'
#' @examples
#'
#' # Instantiate
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
#' # Instantiate
#' x <- rand_uniform()
#' e <- rand_normal()
#' test <- vi_model(prm = list(x = x, e = e),
#'                  prm_type = list(x = "r", e = "r"),
#'                  formula = y ~ 1 + x + x^2 + e)
#' test$prm_type
VI_MODEL$prm_type

#' Closed form expression of `y`
#'
#' @name VI_MODEL$formula
#'
#' @description A quoted formula, will be passed to `CLOSED_FORM$instantiate` to
#' define a closed form expression for `y`.
#'
#' @examples
#'
#' # Instantiate
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
#' @description A quoted formula, will be used to fit the null model.
#'
#' @examples
#'
#' # Instantiate
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
#' @description A quoted formula, will be used to fit the alternative model.
#'
#' @examples
#'
#' # Instantiate
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
#' @param formula Formula. This will be passed to `CLOSED_FORM$instantiate` to
#' define a closed form expression for `y`. Default is `formula = self$formula`.
#' @param null_formula Formula. Formula for fitting the null model. Default is
#' `NULL`.
#' @param alt_formula Formula. Formula for fitting the alternative model.
#' Default is `NULL`.
#' @return Return the object itself.
#'
#' @examples
#'
#' # Instantiate
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
#' # Instantiate
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
#' @description This function store the formula in the environment, and drops all
#' attributes. It is not recommended to modify the formula
#' of `y` after the instance has been built, which may create confusion.
#' @param ... Formulas. Formulas with names.
#' @return Return the object itself.
#'
#' @examples
#'
#' # Instantiate
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

#' Set parameter for the model
#'
#' @name VI_MODEL$set_prm
#'
#' @description This function store the values in the environment and update
#' their values in the closed form expression of `y`. However, if the parameter
#' is not directly used by the closed form expression, user should override
#' this method to correctly set the parameter.
#' @param prm_name List or Vector. Parameter character names.
#' @param prm_val List or Vector. Parameter values.
#' @return Return the object itself.
#'
#' @examples
#'
#' # Instantiate
#' x <- rand_uniform()
#' e <- rand_normal()
#' test <- vi_model(prm = list(a = 1, x = x, e = e),
#'                  prm_type = list(a = "o", x = "r", e = "r"),
#'                  formula = y ~ 1 + a * x + x^2 + e,
#'                  null_formula = y ~ x,
#'                  alt_formula = y ~ x + I(x^2))
#'
#' test
#'
#' test$set_prm("a", 2)
#'
#' test
VI_MODEL$set_prm

#' Generating random values from the expression of `y`
#'
#' @name VI_MODEL$gen
#'
#' @description This function generates random values from the expression of
#' `y`, and keeps all the right hand side information in a data frame.
#' @param n Integer. Number of observations.
#' @param fit_model Boolean. Whether or not to fit a null model to obtain the
#' fitted values and the residuals. Default is `TRUE`.
#' @param test Boolean. Whether or not to test the null model against the
#' alternative model to obtain the test statistic and the p-value.
#' Default is `FALSE`.
#' @param computed List. Default is `NULL`. If it is provided, random variables
#' or random closed form expression will use the values from the list, which
#' makes the expression potentially deterministic.
#' @return A data frame.
#'
#' @examples
#'
#' # Instantiate
#' x <- rand_uniform()
#' e <- rand_normal()
#' test <- vi_model(prm = list(x = x, e = e),
#'                  prm_type = list(x = "r", e = "r"),
#'                  formula = y ~ 1 + x + x^2 + e,
#'                  null_formula = y ~ x,
#'                  alt_formula = y ~ x + I(x^2))
#'
#' dat <- test$gen(10)
#' dat
#'
#' test$gen(10, computed = list(e = dat$e))
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
#' # Instantiate
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
#' `formula`.
#' @param formula Formula. Formula for fitting the model. Default
#' is `formula = self$null_formula`.
#' @param ... Arguments passed to `stats::lm`.
#' @return A fitted linear model.
#'
#' @examples
#'
#' # Instantiate
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

#' Compute the sample based effect size of the simulated data of the defined model
#'
#' @name VI_MODEL$sample_effect_size
#'
#' @description This function computes the sample based effect size of the
#' simulated data of the defined model. This is an abstract method.
#' @param ... Derived class needs to override this method.
#' @return NA
#'
#' @examples
#'
#' VI_MODEL$sample_effect_size()
VI_MODEL$sample_effect_size

#' Compute the effect size of the simulated data or the defined model
#'
#' @name VI_MODEL$average_effect_size
#'
#' @description This function computes the average effect size of the defined model.
#' @param n Number of observations in a plot.
#' @param tol Tolerance of the convergence.
#' @param window_size Window size of the convergence check.
#' @param ... Arguments passed to method `VI_MODEL$sample_effect_size`.
#' @return The average effect size.
VI_MODEL$average_effect_size

#' Plot the residuals vs fitted values plot
#'
#' @name VI_MODEL$plot_resid
#'
#' @description This function generate a residuals vs fitted values plot.
#' @param dat Data frame. A data frame containing `.resid` and `.fitted`.
#' @param alpha Numeric. Alpha of dot. Value between 0 and 1.
#' @param size Numeric. Size of dot. Value between 0 and 1. Default is 0.5.
#' @param stroke Numeric. Stroke of dot. Default is 0.5.
#' @return A ggplot.
#'
#' @examples
#'
#' # Instantiate
#' x <- rand_uniform()
#' e <- rand_normal()
#' test <- vi_model(prm = list(x = x, e = e),
#'                  prm_type = list(x = "r", e = "r"),
#'                  formula = y ~ 1 + x + 10 * x^2 + e,
#'                  null_formula = y ~ x,
#'                  alt_formula = y ~ x + I(x^2))
#'
#' # Residual plot of the null model
#' test$plot_resid(test$gen(1000, fit_model = TRUE))
VI_MODEL$plot_resid

#' Plot the residual Q-Q plot
#'
#' @name VI_MODEL$plot_qq
#'
#' @description This function generate a Q-Q plot for residuals.
#' @param dat Data frame. A data frame containing `.resid`.
#' @return A ggplot.
#'
#' @examples
#'
#' # Instantiate
#' x <- rand_uniform()
#' e <- rand_normal()
#' test <- vi_model(prm = list(x = x, e = e),
#'                  prm_type = list(x = "r", e = "r"),
#'                  formula = y ~ 1 + x + 10 * x^2 + e,
#'                  null_formula = y ~ x,
#'                  alt_formula = y ~ x + I(x^2))
#'
#' # Residual plot of the null model
#' test$plot_qq(test$gen(1000, fit_model = TRUE))
VI_MODEL$plot_qq

#' Plot the fitted model
#'
#' @name VI_MODEL$plot
#'
#' @description This function generates plot for fitted model.
#' @param dat Data frame. A data frame containing correspoding variables.
#' @param type Character. "resid": [VI_MODEL$plot_resid]. Default is
#' `type = "resid"`.
#' @param theme ggtheme. A ggplot theme object.
#' @param alpha Numeric. Alpha of dot. Value between 0 and 1. Default is 1.
#' @param size Numeric. Size of dot. Value between 0 and 1. Default is 0.5.
#' @param stroke Numeric. Stroke of dot. Default is 0.5.
#' @param remove_axis Boolean. Whether or not to remove the axis. Default is
#' `remove_axis = FALSE`.
#' @param remove_legend Boolean. Whether or not to remove the legend. Default is
#' `remove_legend = FALSE`.
#' @param remove_grid_line Boolean. Whether or not to remove the grid lines.
#' Default is `remove_grid_line = FALSE`.
#' @param add_zero_line Boolean. Whether or not to add a zero horizontal line.
#' Default is `add_zero_line = TRUE`.
#' @return A ggplot.
#'
#' @examples
#'
#' # Instantiate
#' x <- rand_uniform()
#' e <- rand_normal()
#' test <- vi_model(prm = list(x = x, e = e),
#'                  prm_type = list(x = "r", e = "r"),
#'                  formula = y ~ 1 + x + 10 * x^2 + e,
#'                  null_formula = y ~ x,
#'                  alt_formula = y ~ x + I(x^2))
#'
#' # Residual plot of the null model
#' test$plot(test$gen(1000, fit_model = TRUE), type = "resid")
#'
#' # Customize the plot
#' test$plot(test$gen(1000, fit_model = TRUE),
#'           type = "resid",
#'           theme = ggplot2::theme_light(),
#'           remove_axis = TRUE,
#'           remove_grid_line = TRUE,
#'           add_zero_line = TRUE)
VI_MODEL$plot

#' Plot the lineup
#'
#' @name VI_MODEL$plot_lineup
#'
#' @description This function plots the lineup.
#' @param dat Data frame. A data frame containing correspoding variables.
#' @param type Character. "resid": [VI_MODEL$plot_resid]. Default is
#' `type = "resid"`.
#' @param ... Arguments passed to [VI_MODEL$plot]
#' @return A ggplot.
#'
#' @examples
#'
#' # Instantiate
#' x <- rand_uniform()
#' e <- rand_normal()
#' test <- vi_model(prm = list(x = x, e = e),
#'                  prm_type = list(x = "r", e = "r"),
#'                  formula = y ~ 1 + x + 10 * x^2 + e,
#'                  null_formula = y ~ x,
#'                  alt_formula = y ~ x + I(x^2))
#'
#' # Lineup of residual plots
#' test$plot_lineup(test$gen_lineup(100), type = "resid")
#'
#' # Customize the plot
#' test$plot_lineup(test$gen_lineup(100),
#'                  type = "resid",
#'                  theme = ggplot2::theme_light(),
#'                  remove_axis = TRUE,
#'                  remove_grid_line = TRUE,
#'                  add_zero_line = TRUE)
VI_MODEL$plot_lineup

#' Residual sum of square of a fitted model
#'
#' @name VI_MODEL$rss
#'
#' @description This function returns the residual sum of square of a fitted
#' model.
#' @return Numeric value.
#'
#' @examples
#'
#' # Instantiate
#' x <- rand_uniform()
#' e <- rand_normal()
#' test <- vi_model(prm = list(x = x, e = e),
#'                  prm_type = list(x = "r", e = "r"),
#'                  formula = y ~ 1 + x + x^2 + e,
#'                  null_formula = y ~ x,
#'                  alt_formula = y ~ x + I(x^2))
#'
#' test$rss(test$fit(test$gen(100)))
VI_MODEL$rss

#' Generate null residuals from a null model
#'
#' @name VI_MODEL$null_resid
#'
#' @description This function uses the data and the null model to generate
#' null residuals. The null residuals are generated by first regressing random
#' noises on the original regressors, then multiply the obtained residuals by
#' original RSS divided by the current RSS. The results are the null residuals.
#' @param dat Data frame. A data frame that used to fit the model.
#' @param mod Linear Model. The null model.
#' @param test Boolean. Whether or not to use `test` on the newly generated
#' data.
#' @return A data frame with updated `y`, `.resid`, and potentially updated
#' test result.
#'
#' @examples
#'
#' # Instantiate
#' x <- rand_uniform()
#' e <- rand_normal()
#' test <- vi_model(prm = list(x = x, e = e),
#'                  prm_type = list(x = "r", e = "r"),
#'                  formula = y ~ 1 + x + x^2 + e,
#'                  null_formula = y ~ x,
#'                  alt_formula = y ~ x + I(x^2))
#'
#' dat <- test$gen(10)
#' test$null_resid(dat, test$fit(dat), test = TRUE)
VI_MODEL$null_resid

#' Generating random values from the expression of `y`, and forms a lineup
#'
#' @name VI_MODEL$gen_lineup
#'
#' @description This function generates random values from the expression of
#' `y`, and keeps all the right hand side information in a data frame.
#' @param n Integer. Number of observations.
#' @param k Integer. Number of plots in the lineup. Default is `k = 20`.
#' @param pos Integer. Position of the true data plot. Default is `pos = NULL`,
#' which means the position is random.
#' @param computed List. Default is `NULL`. If it is provided, random variables
#' or random closed form expression will use the values from the list, which
#' makes the expression potentially deterministic.
#' @return A data frame.
#'
#' @examples
#'
#' # Instantiate
#' x <- rand_uniform()
#' e <- rand_normal()
#' test <- vi_model(prm = list(x = x, e = e),
#'                  prm_type = list(x = "r", e = "r"),
#'                  formula = y ~ 1 + x + x^2 + e,
#'                  null_formula = y ~ x,
#'                  alt_formula = y ~ x + I(x^2))
#'
#' test$gen_lineup(10, k = 3)
#'
#' test$gen_lineup(10, k = 3, computed = list(e = 1:10))
VI_MODEL$gen_lineup


# CUBIC_MODEL -------------------------------------------------------------

#' CUBIC_MODEL class environment
#'
#' @name CUBIC_MODEL
#'
#' @description This is the class of visual inference cubic linear model,
#' inherited from [VI_MODEL].
#' @format An environment with S3 class `bandicoot_oop`.
#' @seealso Parent class: [VI_MODEL]
#' \cr
#' \cr
#' New attributes: [CUBIC_MODEL$formula],
#' [CUBIC_MODEL$null_formula], [CUBIC_MODEL$alt_formula],
#' \cr
#' \cr
#' New methods: [CUBIC_MODEL$..init..], [CUBIC_MODEL$E],
#' [CUBIC_MODEL$sample_effect_size], [CUBIC_MODEL$set_prm]
#' @export
CUBIC_MODEL <- new.env()

#' Closed form expression of `y`
#'
#' @name CUBIC_MODEL$formula
#'
#' @description A quoted formula, will be passed to `CLOSED_FORM$instantiate` to
#' define a closed form expression for `y`.
#'
#' @examples
#'
#' CUBIC_MODEL$formula
CUBIC_MODEL$formula

#' Formula for fitting the null model
#'
#' @name CUBIC_MODEL$null_formula
#'
#' @description Quoted formula for fitting the null model.
#'
#' @examples
#'
#' CUBIC_MODEL$null_formula
CUBIC_MODEL$null_formula

#' Formula for fitting the alternative model
#'
#' @name CUBIC_MODEL$alt_formula
#'
#' @description Quoted formula for fitting the alternative model.
#'
#' @examples
#'
#' CUBIC_MODEL$alt_formula
CUBIC_MODEL$alt_formula

#' Initialization method
#'
#' @name CUBIC_MODEL$..init..
#'
#' @description This function will be called after an instance is built. User
#' input will be stored in the environment. The response variable of this model
#' is `y`. The formula of y is defined in [CUBIC_MODEL$formula], the null
#' formula is defined in [CUBIC_MODEL$null_formula], the alternative is
#' defined in [CUBIC_MODEL$alt_formula].
#' @param a Numeric. Default is `a = 1`.
#' @param b Numeric. Default is `b = 1`.
#' @param c Numeric. Default is `c = 1`.
#' @param sigma Positive numeric. Default is `sigma = 1`.
#' @param x Random variable or closed form expression. Default is
#' `x = rand_uniform(-1, 1, env = new.env(parent = parent.env(self)))`.
#' @param z Random variable or closed form expression. Default is
#' `z = rand_uniform(-1, 1, env = new.env(parent = parent.env(self)))`.
#' @param e Random variable or closed form expression. Default is
#' `e = rand_normal(0, sigma, env = new.env(parent = parent.env(self)))`.
#' @return Return the object itself.
#'
#' @examples
#'
#' # Instantiate
#' x <- rand_uniform()
#' z <- rand_uniform()
#' e <- rand_normal()
#'
#' test <- cubic_model(a = 200, b = 200, c = 1, x = x, z = z, e = e)
#'
#' test
#'
#' # Generate data
#' test$gen(10)
#'
#' # Generate lineup
#' test$gen_lineup(10, k = 3)
#'
#' # Plot the lineup
#' test$plot_lineup(test$gen_lineup(100))
CUBIC_MODEL$..init..

#' Set parameter for the model
#'
#' @name CUBIC_MODEL$set_prm
#'
#' @description This function store the values in the environment and update
#' their values in the closed form expression of `y`, except the parameter
#' `sigma`. For parameter `sigma`, its value will be updated, and the
#' corresponding value in `e` will be updated.
#' @param prm_name List or Vector. Parameter character names.
#' @param prm_val List or Vector. Parameter values.
#' @return Return the object itself.
#'
#' @examples
#'
#' # Instantiate
#' mod <- cubic_model(a = -1, b = 1, c = 1, sigma = 0.5)
#'
#' mod
#'
#' mod$set_prm("a", 2)
#'
#' mod
#'
#' mod$set_prm("sigma", 1)
#'
#' mod
CUBIC_MODEL$set_prm


#' Expectation of the residuals
#'
#' @name CUBIC_MODEL$E
#'
#' @description This function calculate the expectation of the residuals by the
#' use of the Frisch–Waugh–Lovell theorem.
#' @param dat Dataframe/List. List contains variable `x` and `z`.
#' @param a Numeric. Default is `a = self$prm$a`.
#' @param b Numeric. Default is `b = self$prm$b`.
#' @param c Numeric. Default is `c = self$prm$c`.
#' @return A vector of numeric expectations.
#'
#' @examples
#'
#' mod <- cubic_model(-1, 1, 1, 0.5)
#' dat <- mod$gen(1000, fit_model = TRUE)
#' dat$exp <- mod$E(dat)
#' mod$plot(dat) + ggplot2::geom_point(ggplot2::aes(.fitted, exp),
#'                                     col = "red",
#'                                     alpha = 0.6)
CUBIC_MODEL$E

#' Compute the sample baased effect size of the simulated data
#'
#' @name CUBIC_MODEL$sample_effect_size
#'
#' @description This function computes the sample based effect size of the simulated data.
#' @param dat Dataframe/List. List contains variable `x` and `z`.
#' @param a Numeric. Default is `a = self$prm$a`.
#' @param b Numeric. Default is `b = self$prm$b`.
#' @param c Numeric. Default is `c = self$prm$c`.
#' @param sigma Positive numeric. Default is `sigma = self$prm$sigma`.
#' @return A numeric value.
#'
#' @examples
#'
#' mod <- cubic_model(-1, 1, 1, 0.5)
#' dat <- mod$gen(1000, fit_model = TRUE)
#' mod$sample_effect_size(dat)
CUBIC_MODEL$sample_effect_size

# HETER_MODEL -------------------------------------------------------------


#' HETER_MODEL class environment
#'
#' @name HETER_MODEL
#'
#' @description This is the class of visual inference heteroskedasticity
#' linear model, inherited from [VI_MODEL].
#' @format An environment with S3 class `bandicoot_oop`.
#' @seealso Parent class: [VI_MODEL]
#' \cr
#' \cr
#' New attributes: [HETER_MODEL$formula],
#' [HETER_MODEL$null_formula],
#' [HETER_MODEL$alt_formula]
#' \cr
#' \cr
#' New methods: [HETER_MODEL$..init..], [HETER_MODEL$test],
#' [HETER_MODEL$sample_effect_size]
#' @export
HETER_MODEL <- new.env()


#' Closed form expression of `y`
#'
#' @name HETER_MODEL$formula
#'
#' @description A quoted formula, will be passed to `CLOSED_FORM$instantiate` to
#' define a closed form expression for `y`.
#'
#' @examples
#'
#' HETER_MODEL$formula
HETER_MODEL$formula

#' Formula for fitting the null model
#'
#' @name HETER_MODEL$null_formula
#'
#' @description Quoted formula for fitting the null model.
#'
#' @examples
#'
#' HETER_MODEL$null_formula
HETER_MODEL$null_formula

#' Formula for fitting the alternative model
#'
#' @name HETER_MODEL$alt_formula
#'
#' @description Quoted formula for fitting the alternative model.
#'
#' @examples
#'
#' HETER_MODEL$alt_formula
HETER_MODEL$alt_formula

#' Initialization method
#'
#' @name HETER_MODEL$..init..
#'
#' @description This function will be called after an instance is built. User
#' input will be stored in the environment. The response variable of this model
#' is `y`. The formula of y is defined in [HETER_MODEL$formula],
#' the null formula is defined in [HETER_MODEL$null_formula].
#' @param a Numeric. Default is `a = 0`.
#' @param b Numeric. Default is `b = 1`.
#' @param x Random variable or closed form expression. Default is
#' `x = rand_uniform(-1, 1, env = new.env(parent = parent.env(self)))`.
#' @param e Random variable or closed form expression. Default is
#' `e = rand_normal(0, 1, env = new.env(parent = parent.env(self)))`.
#' @return Return the object itself.
#'
#' @examples
#'
#' # Instantiate
#' test <- heter_model(a = 0, b = 20)
#'
#' test
#'
#' # Generate data
#' test$gen(10)
#'
#' # Generate lineup
#' test$gen_lineup(10, k = 3)
#'
#' # Plot the lineup
#' test$plot_lineup(test$gen_lineup(100))
HETER_MODEL$..init..

#' Test the null model
#'
#' @name HETER_MODEL$test
#'
#' @description This function test the null model.
#' In this class, the test is a BP-test computed using [lmtest::bptest]. The
#' variance formula is `~ x + I(x^2)`.
#' @param dat Data frame. A data frame containing all variables needed by the
#' `null_formula`.
#' @param null_formula Formula. Formula for fitting the null model. Default
#' is `null_formula = self$null_formula`.
#' @return A list containing the test name, the test statistic and the p-value.
#'
#' @examples
#'
#' # Instantiate
#' x <- rand_uniform()
#' e <- rand_normal()
#' test <- heter_model(a = 0, b = 100)
#'
#'
#' dat <- test$gen(100000)
#' test$test(dat)
HETER_MODEL$test


#' Compute the sample based effect size of the simulated data
#'
#' @name HETER_MODEL$sample_effect_size
#'
#' @description This function computes the sample based effect size of the simulated data.
#' @param dat Dataframe. The number of rows of the data frame will be used.
#' @param a Numeric. Default is `a = self$prm$a`.
#' @param b Numeric. Default is `b = self$prm$b`.
#' @param type Character. Type of the effect size. Default is `type = NULL`.
#' @return A single numeric value.
#'
#' @examples
#'
#' mod <- heter_model(a = 0, b = 16)
#' mod$sample_effect_size(mod$gen(100))
HETER_MODEL$sample_effect_size



# SIMPLE_CUBIC_MODEL ------------------------------------------------------

#' SIMPLE_CUBIC_MODEL class environment
#'
#' @name SIMPLE_CUBIC_MODEL
#'
#' @description This is the class of visual inference simple cubic linear model,
#' inherited from [VI_MODEL].
#' @format An environment with S3 class `bandicoot_oop`.
#' @seealso Parent class: [VI_MODEL]
#' \cr
#' \cr
#' New attributes: [SIMPLE_CUBIC_MODEL$formula],
#' [SIMPLE_CUBIC_MODEL$null_formula], [SIMPLE_CUBIC_MODEL$alt_formula],
#' \cr
#' \cr
#' New methods: [SIMPLE_CUBIC_MODEL$..init..], [SIMPLE_CUBIC_MODEL$E],
#' [SIMPLE_CUBIC_MODEL$sample_effect_size], [SIMPLE_CUBIC_MODEL$set_prm]
#' @export
SIMPLE_CUBIC_MODEL <- new.env()

#' Closed form expression of `y`
#'
#' @name SIMPLE_CUBIC_MODEL$formula
#'
#' @description A quoted formula, will be passed to `CLOSED_FORM$instantiate` to
#' define a closed form expression for `y`.
#'
#' @examples
#'
#' SIMPLE_CUBIC_MODEL$formula
SIMPLE_CUBIC_MODEL$formula

#' Formula for fitting the null model
#'
#' @name SIMPLE_CUBIC_MODEL$null_formula
#'
#' @description Quoted formula for fitting the null model.
#'
#' @examples
#'
#' SIMPLE_CUBIC_MODEL$null_formula
SIMPLE_CUBIC_MODEL$null_formula

#' Formula for fitting the alternative model
#'
#' @name SIMPLE_CUBIC_MODEL$alt_formula
#'
#' @description Quoted formula for fitting the alternative model.
#'
#' @examples
#'
#' SIMPLE_CUBIC_MODEL$alt_formula
SIMPLE_CUBIC_MODEL$alt_formula

#' Initialization method
#'
#' @name SIMPLE_CUBIC_MODEL$..init..
#'
#' @description This function will be called after an instance is built. User
#' input will be stored in the environment. The response variable of this model
#' is `y`. The formula of y is defined in [SIMPLE_CUBIC_MODEL$formula], the null
#' formula is defined in [SIMPLE_CUBIC_MODEL$null_formula], the alternative is
#' defined in [SIMPLE_CUBIC_MODEL$alt_formula].
#' @param a Numeric. Default is `a = 1`.
#' @param b Numeric. Default is `b = 1`.
#' @param sigma Positive numeric. Default is `sigma = 1`.
#' @param x Random variable or closed form expression. Default is
#' `x = rand_uniform(-1, 1, env = new.env(parent = parent.env(self)))`.
#' @param e Random variable or closed form expression. Default is
#' `e = rand_normal(0, sigma, env = new.env(parent = parent.env(self)))`.
#' @return Return the object itself.
#'
#' @examples
#'
#' # Instantiate
#' x <- rand_uniform()
#' e <- rand_normal()
#'
#' test <- simple_cubic_model(a = 200, b = 200, x = x, e = e)
#'
#' test
#'
#' # Generate data
#' test$gen(10)
#'
#' # Generate lineup
#' test$gen_lineup(10, k = 3)
#'
#' # Plot the lineup
#' test$plot_lineup(test$gen_lineup(100))
SIMPLE_CUBIC_MODEL$..init..

#' Set parameter for the model
#'
#' @name SIMPLE_CUBIC_MODEL$set_prm
#'
#' @description This function store the values in the environment and update
#' their values in the closed form expression of `y`, except the parameter
#' `sigma`. For parameter `sigma`, its value will be updated, and the
#' corresponding value in `e` will be updated.
#' @param prm_name List or Vector. Parameter character names.
#' @param prm_val List or Vector. Parameter values.
#' @return Return the object itself.
#'
#' @examples
#'
#' # Instantiate
#' mod <- simple_cubic_model(a = -1, b = 1, sigma = 0.5)
#'
#' mod
#'
#' mod$set_prm("a", 2)
#'
#' mod
#'
#' mod$set_prm("sigma", 1)
#'
#' mod
SIMPLE_CUBIC_MODEL$set_prm


#' Expectation of the residuals
#'
#' @name SIMPLE_CUBIC_MODEL$E
#'
#' @description This function calculate the expectation of the residuals by the
#' use of the Frisch–Waugh–Lovell theorem.
#' @param dat Dataframe/List. List contains variable `x` and `z`.
#' @param a Numeric. Default is `a = self$prm$a`.
#' @param b Numeric. Default is `b = self$prm$b`.
#' @return A vector of numeric expectations.
#'
#' @examples
#'
#' mod <- simple_cubic_model(-1, 1, 0.5)
#' dat <- mod$gen(1000, fit_model = TRUE)
#' dat$exp <- mod$E(dat)
#' mod$plot(dat) + ggplot2::geom_point(ggplot2::aes(.fitted, exp),
#'                                     col = "red",
#'                                     alpha = 0.6)
SIMPLE_CUBIC_MODEL$E

#' Compute the sample based effect size of the simulated data
#'
#' @name SIMPLE_CUBIC_MODEL$sample_effect_size
#'
#' @description This function computes the sample based effect size of the simulated data.
#' @param dat Dataframe/List. List contains variable `x`.
#' @param a Numeric. Default is `a = self$prm$a`.
#' @param b Numeric. Default is `b = self$prm$b`.
#' @param sigma Positive numeric. Default is `sigma = self$prm$sigma`.
#' @return A numeric value.
#'
#' @examples
#'
#' mod <- simple_cubic_model(-1, 1, 0.5)
#' dat <- mod$gen(1000, fit_model = TRUE)
#' mod$sample_effect_size(dat)
SIMPLE_CUBIC_MODEL$sample_effect_size



# QUARTIC_MODEL -----------------------------------------------------------

#' QUARTIC_MODEL class environment
#'
#' @name QUARTIC_MODEL
#'
#' @description This is the class of visual inference quartic linear model,
#' inherited from [VI_MODEL].
#' @format An environment with S3 class `bandicoot_oop`.
#' @seealso Parent class: [VI_MODEL]
#' \cr
#' \cr
#' New attributes: [QUARTIC_MODEL$formula],
#' [QUARTIC_MODEL$null_formula], [QUARTIC_MODEL$alt_formula],
#' \cr
#' \cr
#' New methods: [QUARTIC_MODEL$..init..], [QUARTIC_MODEL$E],
#' [QUARTIC_MODEL$sample_effect_size], [QUARTIC_MODEL$set_prm]
#' @export
QUARTIC_MODEL <- new.env()

#' Closed form expression of `y`
#'
#' @name QUARTIC_MODEL$formula
#'
#' @description A quoted formula, will be passed to `CLOSED_FORM$instantiate` to
#' define a closed form expression for `y`.
#'
#' @examples
#'
#' QUARTIC_MODEL$formula
QUARTIC_MODEL$formula

#' Formula for fitting the null model
#'
#' @name QUARTIC_MODEL$null_formula
#'
#' @description Quoted formula for fitting the null model.
#'
#' @examples
#'
#' QUARTIC_MODEL$null_formula
QUARTIC_MODEL$null_formula

#' Formula for fitting the alternative model
#'
#' @name QUARTIC_MODEL$alt_formula
#'
#' @description Quoted formula for fitting the alternative model.
#'
#' @examples
#'
#' QUARTIC_MODEL$alt_formula
QUARTIC_MODEL$alt_formula

#' Initialization method
#'
#' @name QUARTIC_MODEL$..init..
#'
#' @description This function will be called after an instance is built. User
#' input will be stored in the environment. The response variable of this model
#' is `y`. The formula of y is defined in [QUARTIC_MODEL$formula], the null
#' formula is defined in [QUARTIC_MODEL$null_formula], the alternative is
#' defined in [QUARTIC_MODEL$alt_formula].
#' @param a Numeric. Default is `a = 1`.
#' @param b Numeric. Default is `b = 1`.
#' @param c Numeric. Default is `c = 1`.
#' @param sigma Positive numeric. Default is `sigma = 1`.
#' @param x Random variable or closed form expression. Default is
#' `x = rand_uniform(-1, 1, env = new.env(parent = parent.env(self)))`.
#' @param e Random variable or closed form expression. Default is
#' `e = rand_normal(0, sigma, env = new.env(parent = parent.env(self)))`.
#' @return Return the object itself.
#'
#' @examples
#'
#' # Instantiate
#' x <- rand_uniform()
#' e <- rand_normal()
#'
#' test <- quartic_model(a = 200, b = 200, c = 20, x = x, e = e)
#'
#' test
#'
#' # Generate data
#' test$gen(10)
#'
#' # Generate lineup
#' test$gen_lineup(10, k = 3)
#'
#' # Plot the lineup
#' test$plot_lineup(test$gen_lineup(100))
QUARTIC_MODEL$..init..

#' Set parameter for the model
#'
#' @name QUARTIC_MODEL$set_prm
#'
#' @description This function store the values in the environment and update
#' their values in the closed form expression of `y`, except the parameter
#' `sigma`. For parameter `sigma`, its value will be updated, and the
#' corresponding value in `e` will be updated.
#' @param prm_name List or Vector. Parameter character names.
#' @param prm_val List or Vector. Parameter values.
#' @return Return the object itself.
#'
#' @examples
#'
#' # Instantiate
#' mod <- quartic_model(a = -1, b = 1, c = 1, sigma = 0.5)
#'
#' mod
#'
#' mod$set_prm("a", 2)
#'
#' mod
#'
#' mod$set_prm("sigma", 1)
#'
#' mod
QUARTIC_MODEL$set_prm


#' Expectation of the residuals
#'
#' @name QUARTIC_MODEL$E
#'
#' @description This function calculate the expectation of the residuals by the
#' use of the Frisch–Waugh–Lovell theorem.
#' @param dat Dataframe/List. List contains variable `x` and `z`.
#' @param a Numeric. Default is `a = self$prm$a`.
#' @param b Numeric. Default is `b = self$prm$b`.
#' @param c Numeric. Default is `c = self$prm$c`.
#' @return A vector of numeric expectations.
#'
#' @examples
#'
#' mod <- quartic_model(-1, 1, 1, 0.5)
#' dat <- mod$gen(1000, fit_model = TRUE)
#' dat$exp <- mod$E(dat)
#' mod$plot(dat) + ggplot2::geom_point(ggplot2::aes(.fitted, exp),
#'                                     col = "red",
#'                                     alpha = 0.6)
QUARTIC_MODEL$E

#' Compute the sample based effect size of the simulated data
#'
#' @name QUARTIC_MODEL$sample_effect_size
#'
#' @description This function computes the sample based effect size of the simulated data.
#' @param dat Dataframe/List. List contains variable `x`.
#' @param a Numeric. Default is `a = self$prm$a`.
#' @param b Numeric. Default is `b = self$prm$b`.
#' @param c Numeric. Default is `c = self$prm$c`.
#' @param sigma Positive numeric. Default is `sigma = self$prm$sigma`.
#' @return A numeric value.
#'
#' @examples
#'
#' mod <- quartic_model(-1, 1, 1, 0.5)
#' dat <- mod$gen(1000, fit_model = TRUE)
#' mod$sample_effect_size(dat)
QUARTIC_MODEL$sample_effect_size


# POLY_MODEL --------------------------------------------------------------

#' POLY_MODEL class environment
#'
#' @name POLY_MODEL
#'
#' @description This is the class of visual inference orthogonal polynomial
#' linear model, inherited from [VI_MODEL].
#' @format An environment with S3 class `bandicoot_oop`.
#' @seealso Parent class: [VI_MODEL]
#' \cr
#' \cr
#' New attributes: [POLY_MODEL$formula],
#' [POLY_MODEL$null_formula], [POLY_MODEL$alt_formula], [POLY_MODEL$z_formula],
#' [POLY_MODEL$raw_z_formula]
#' \cr
#' \cr
#' New methods: [POLY_MODEL$..init..], [POLY_MODEL$test], [POLY_MODEL$E],
#' [POLY_MODEL$sample_effect_size], [POLY_MODEL$set_prm], [POLY_MODEL$hermite]
#' @export
POLY_MODEL <- new.env()

#' Closed form expression of `y`
#'
#' @name POLY_MODEL$formula
#'
#' @description A quoted formula, will be passed to `CLOSED_FORM$instantiate` to
#' define a closed form expression for `y`.
#'
#' @examples
#'
#' POLY_MODEL$formula
POLY_MODEL$formula

#' Formula for fitting the null model
#'
#' @name POLY_MODEL$null_formula
#'
#' @description Quoted formula for fitting the null model.
#'
#' @examples
#'
#' POLY_MODEL$null_formula
POLY_MODEL$null_formula

#' Formula for fitting the alternative model
#'
#' @name POLY_MODEL$alt_formula
#'
#' @description Quoted formula for fitting the alternative model.
#'
#' @examples
#'
#' POLY_MODEL$alt_formula
POLY_MODEL$alt_formula

#' Formula for the raw orthogonal polynomial term `raw_z`
#'
#' @name POLY_MODEL$raw_z_formula
#'
#' @description Quoted formula for the raw orthogonal polynomial term `raw_z`.
#'
#' @examples
#'
#' POLY_MODEL$raw_z_formula
POLY_MODEL$raw_z_formula

#' Formula for the scaled orthogonal polynomial term `z`
#'
#' @name POLY_MODEL$z_formula
#'
#' @description Quoted formula for the scaled orthogonal polynomial term `z`.
#'
#' @examples
#'
#' POLY_MODEL$z_formula
POLY_MODEL$z_formula


#' Initialization method
#'
#' @name POLY_MODEL$..init..
#'
#' @description This function will be called after an instance is built. User
#' input will be stored in the environment. The response variable of this model
#' is `y`. The formula of y is defined in [POLY_MODEL$formula], the null
#' formula is defined in [POLY_MODEL$null_formula], the alternative is
#' defined in [POLY_MODEL$alt_formula]. The formula for the raw
#' orthogonal polynomial term is defined in [POLY_MODEL$raw_z_formula], and
#' the scaled orthogonal polynomial term is defined in [POLY_MODEL$z_formula].
#' @param shape Integer. The shape of the orthogonal polynomial used in the model.
#' Note it should be a value between 1 to 4. Default is `shape = 1`.
#' @param sigma Positive numeric. Default is `sigma = 1`.
#' @param include_z Boolean. Whether or not to include `z` in the formula of `y`.
#' Default is `include_z = TRUE`.
#' @param x Random variable or closed form expression. Default is
#' `x = rand_uniform(-1, 1, env = new.env(parent = parent.env(self)))`.
#' @param e Random variable or closed form expression. Default is
#' `e = rand_normal(0, sigma, env = new.env(parent = parent.env(self)))`.
#' @return Return the object itself.
#'
#' @examples
#'
#' # Instantiate
#' x <- rand_uniform()
#' e <- rand_normal(sigma = 0.5)
#'
#' test <- poly_model(shape = 1, x = x, e = e)
#'
#' test
#'
#' # Generate data
#' test$gen(10)
#'
#' # Generate lineup
#' test$gen_lineup(10, k = 3)
#'
#' # Plot the lineup
#' test$plot_lineup(test$gen_lineup(100))
#'
#' test <- poly_model(shape = 1, include_z = FALSE, x = x, e = e)
#' test$plot_lineup(test$gen_lineup(100))
#'
#' test <- poly_model(shape = 2, x = x, e = e)
#' test$plot_lineup(test$gen_lineup(100))
#'
#' test <- poly_model(shape = 3, x = x, e = e)
#' test$plot_lineup(test$gen_lineup(100))
#'
#' test <- poly_model(shape = 4, x = x, e = e)
#' test$plot_lineup(test$gen_lineup(100))
POLY_MODEL$..init..


#' Test the null model
#'
#' @name POLY_MODEL$test
#'
#' @description This function test the null model.
#' In this class, the test is either a F-test computed using [stats::anova] or a
#' RESET test computed using [lmtest::resettest].
#' @param dat Data frame. A data frame containing all variables needed by the
#' `null_formula`.
#' @param null_formula Formula. Formula for fitting the null model. Default
#' is `null_formula = self$null_formula`.
#' @param alt_formula Formula. Formula for fitting the alternative model. Only
#' used in F-test. Default is `alt_formula = self$alt_formula`.
#' @param test Character. A string representing the test. Either "F" or "RESET".
#' @param power Integer. A vector of integers representing the power of the
#' variables that should be included. Only used in RESET test. Default is 2:3.
#' @param power_type Character. Argument passed to [lmtest::resettest]. "fitted",
#' "regressor" or "princomp". Only used in RESET test. Default is "fitted".
#' @return A list containing the test name, the test statistic and the p-value.
#'
#' @examples
#'
#' # Instantiate
#' mod <- poly_model(shape = 2, sigma = 0.5)
#'
#' mod
#'
#' dat <- mod$gen(100000)
#'
#' # F-test
#' mod$test(dat, test = "F")
#'
#' # RESET test
#' mod$test(dat, test = "RESET", power = 2:4)
POLY_MODEL$test

#' Set parameter for the model
#'
#' @name POLY_MODEL$set_prm
#'
#' @description This function store the values in the environment and update
#' their values in the closed form expression of `y`, except the parameter
#' `sigma`, `shape` and `raw_z`. For parameter `sigma`, its value will be updated,
#' and the corresponding value in `e` will be updated. For parameter `shape`, its
#' value will be updated, and the corresponding value in `raw_z` will be
#' updated. For parameter `raw_z`, its value will be updated, and the
#' corresponding value in `z` will be updated.
#' @param prm_name List or Vector. Parameter character names.
#' @param prm_val List or Vector. Parameter values.
#' @return Return the object itself.
#'
#' @examples
#'
#' # Instantiate
#' mod <- poly_model(shape = 2, sigma = 0.5)
#'
#' mod
#'
#' mod$set_prm("shape", 4)
#'
#' mod
#'
#' mod$set_prm("sigma", 1)
#'
#' mod
POLY_MODEL$set_prm


#' Expectation of the residuals
#'
#' @name POLY_MODEL$E
#'
#' @description This function calculate the expectation of the residuals by the
#' use of the Frisch–Waugh–Lovell theorem.
#' @param dat Dataframe/List. List contains variable `x` and `z`.
#' @return A vector of numeric expectations.
#'
#' @examples
#'
#' mod <- poly_model(4, 0.5)
#' dat <- mod$gen(1000, fit_model = TRUE)
#' dat$exp <- mod$E(dat)
#' mod$plot(dat) + ggplot2::geom_point(ggplot2::aes(.fitted, exp),
#'                                     col = "red",
#'                                     alpha = 0.6)
POLY_MODEL$E

#' Compute the sample based effect size of the simulated data
#'
#' @name POLY_MODEL$sample_effect_size
#'
#' @description This function computes the sample based effect size of the simulated data.
#' @param dat Dataframe/List. List contains variable `x` and `z`.
#' @param sigma Positive numeric. Default is `sigma = self$prm$sigma`.
#' @param include_z Boolean. Default is `include_z = self$prm$include_z`.
#' @param shape Positive integer. Default is `shape = self$prm$shape`.
#' @param type Character. Type of the effect size measure. Default is
#' `type = kl`.
#' @return A numeric value.
#'
#' @examples
#'
#' mod <- poly_model(4, 0.5)
#' dat <- mod$gen(1000, fit_model = TRUE)
#' mod$sample_effect_size(dat)
POLY_MODEL$sample_effect_size

#' Hermite polynomial functions
#'
#' @name POLY_MODEL$hermite
#'
#' @description This function returns a Hermite polynomial function defined
#' using [mpoly::hermite].
#' @param shape Integer. A value between 1 to 4.
#' @return A function.
#'
#' @examples
#'
#' POLY_MODEL$hermite(1)
#' POLY_MODEL$hermite(2)
#' POLY_MODEL$hermite(3)
#' POLY_MODEL$hermite(4)
POLY_MODEL$hermite



# AR1_MODEL ---------------------------------------------------------------

#' AR1_MODEL class environment
#'
#' @name AR1_MODEL
#'
#' @description This is the class of visual inference orthogonal polynomial
#' linear model, inherited from [VI_MODEL].
#' @format An environment with S3 class `bandicoot_oop`.
#' @seealso Parent class: [VI_MODEL]
#' \cr
#' \cr
#' New attributes: [AR1_MODEL$formula],
#' [AR1_MODEL$null_formula], [AR1_MODEL$alt_formula]
#' \cr
#' \cr
#' New methods: [AR1_MODEL$..init..], [AR1_MODEL$test], [AR1_MODEL$set_prm], [AR1_MODEL$ar1]
#' @export
AR1_MODEL <- new.env()

#' Initialization method
#'
#' @name AR1_MODEL$..init..
#'
#' @description This function will be called after an instance is built. User
#' input will be stored in the environment. The response variable of this model
#' is `y`. The formula of y is defined in [AR1_MODEL$formula], the null
#' formula is defined in [AR1_MODEL$null_formula], the alternative is
#' defined in [AR1_MODEL$alt_formula].
#' @param phi Numeric. The parameter for the `AR(1)` term. Default is `phi = 0.5`.
#' @param sigma Positive numeric. Default is `sigma = 1`.
#' @param x Random variable or closed form expression. Default is
#' `x = rand_uniform(-1, 1, env = new.env(parent = parent.env(self)))`.
#' @param e Random variable or closed form expression. Default is
#' `e = rand_normal(0, sigma, env = new.env(parent = parent.env(self)))`.
#' @return Return the object itself.
#'
#' @examples
#'
#' # Instantiate
#' x <- rand_uniform()
#' e <- rand_normal(sigma = 0.5)
#'
#' test <- ar1_model(phi = 0.9, x = x, e = e)
#'
#' test
#'
#' # Generate data
#' test$gen(10)
#'
#' # Generate lineup
#' test$gen_lineup(10, k = 3)
#'
#' # Plot the lineup
#' test$plot_lineup(test$gen_lineup(100))
#'
#' test <- ar1_model(phi = 0.1, x = x, e = e)
#' test$plot_lineup(test$gen_lineup(100))
#'
#' test <- ar1_model(phi = 0.3, x = x, e = e)
#' test$plot_lineup(test$gen_lineup(100))
#'
#' test <- ar1_model(phi = 0.5, x = x, e = e)
#' test$plot_lineup(test$gen_lineup(100))
#'
#' test <- ar1_model(phi = 0.7, x = x, e = e)
#' test$plot_lineup(test$gen_lineup(100))
AR1_MODEL$..init..


# NON_NORMAL_MODEL --------------------------------------------------------


#' NON_NORMAL_MODEL class environment
#'
#' @name NON_NORMAL_MODEL
#'
#' @description This is the class of visual inference orthogonal polynomial
#' linear model, inherited from [VI_MODEL].
#' @format An environment with S3 class `bandicoot_oop`.
#' @seealso Parent class: [VI_MODEL]
#' \cr
#' \cr
#' New attributes: [NON_NORMAL_MODEL$formula],
#' [NON_NORMAL_MODEL$null_formula], [NON_NORMAL_MODEL$alt_formula]
#' \cr
#' \cr
#' New methods: [NON_NORMAL_MODEL$..init..], [NON_NORMAL_MODEL$test]
#' @export
NON_NORMAL_MODEL <- new.env()

#' Initialization method
#'
#' @name NON_NORMAL_MODEL$..init..
#'
#' @description This function will be called after an instance is built. User
#' input will be stored in the environment. The response variable of this model
#' is `y`. The formula of y is defined in [NON_NORMAL_MODEL$formula], the null
#' formula is defined in [NON_NORMAL_MODEL$null_formula], the alternative is
#' defined in [NON_NORMAL_MODEL$alt_formula].
#' @param x Random variable or closed form expression. Default is
#' `x = rand_uniform(-1, 1, env = new.env(parent = parent.env(self)))`.
#' @param e Random variable or closed form expression. Default is
#' `e = rand_lognormal(0, sigma, env = new.env(parent = parent.env(self)))`.
#' @return Return the object itself.
#'
#' @examples
#'
#' # Instantiate
#' x <- rand_uniform()
#' e <- rand_lognormal(sigma = 0.5)
#'
#' test <- non_normal_model(x = x, e = e)
#'
#' test
#'
#' # Generate data
#' test$gen(10)
#'
#' # Generate lineup
#' test$gen_lineup(10, k = 3)
#'
#' # Plot the lineup
#' test$plot_lineup(test$gen_lineup(100))
#'
#' test <- non_normal_model(x = x, e = rand_lognormal(sigma = 0.1))
#' test$plot_lineup(test$gen_lineup(100))
#'
#' test <- non_normal_model(x = x, e = rand_lognormal(sigma = 0.5))
#' test$plot_lineup(test$gen_lineup(100))
#'
#' test <- non_normal_model(x = x, e = rand_lognormal(sigma = 1))
#' test$plot_lineup(test$gen_lineup(100))
#'
#' test <- non_normal_model(x = x, e = rand_lognormal(sigma = 2))
#' test$plot_lineup(test$gen_lineup(100))
NON_NORMAL_MODEL$..init..
