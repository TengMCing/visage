
<!-- README.md is generated from README.Rmd. Please edit that file -->

# visage

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/TengMCing/visage/branch/master/graph/badge.svg)](https://app.codecov.io/gh/TengMCing/visage?branch=master)
<!-- badges: end -->

The goal of visage is to provide an lightweight OOP system and a set of
tools for running visual inference experiments.

## Installation

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("TengMCing/visage")
```

``` r
library(visage)
```

## Table of Content

1.  [Visual inference cubic linear model class
    `CUBIC_MODEL`](#1-visual-inference-cubic-linear-model-class-cubic_model)
2.  [Visual inference heteroskedasticity linear model class
    `HETER_MODEL`](#2-visual-inference-heteroskedasticity-linear-model-class-heter_model)
3.  [Closed form expression
    `CLOSED_FORM`](#3-closed-form-expression-closed_form)

## 1. Visual inference cubic linear model class `CUBIC_MODEL`

**1.1. Define a model**

A visual inference cubic linear model is defined as:

``` r
CUBIC_MODEL$formula
#> y ~ 1 + (2 - c) * x + c * z + a * (((2 - c) * x)^2 + (c * z)^2) + 
#>     b * (((2 - c) * x)^3 + (c * z)^3) + e
```

where `x` and `z` are random variables, `a`, `b` and `c` are parameters
and `e` is the random error term.

`cubic_model()` is a cubic model class constructor, which takes
arguments `a`, `b`, `c`, `sigma`, `x` and `z`, where the first four are
numeric values defined above, and `x` and `z` are random variable
instances created by the random variable abstract base class constructor
`rand_var()`. If we would like `x` and `z` to be uniform random
variables ranged from −1 to 1, it can be achieved by using the uniform
random variable class constructor `rand_uniform()`. It only takes two
arguments which are the lower bound and the upper bound of the support.

``` r
mod <- cubic_model(a = -3, b = -3, c = 1, sigma = 1,
                   x = rand_uniform(-1, 1), z = rand_uniform(-1, 1))
mod
#> 
#> ── <CUBIC_MODEL object>
#> y = 1 + (2 - c) * x + c * z + a * (((2 - c) * x)^2 + (c * z)^2) + b * (((2 - c) * x)^3 + (c * z)^3) + e
#>  - x: <RAND_UNIFORM object>
#>    [a: -1, b: 1]
#>  - z: <RAND_UNIFORM object>
#>    [a: -1, b: 1]
#>  - e: <RAND_NORMAL object>
#>    [mu: 0, sigma: 1]
#>  - a: -3
#>  - b: -3
#>  - c: 1
#>  - sigma: 1
```

**1.2. Generate random values from the model**

An instance of cubic model class contains methods of simulating data and
making residual plot. Method `gen()` returns a data frame containing
realizations of `x`, `z`, `y` and `e` simulated from the model. The
number of realizations depends on the integer argument `n`. In addition,
a null model will be fitted using the simulated data and residuals and
fitted values will be included in the returned data frame.

The null model is defined as:

``` r
CUBIC_MODEL$null_formula
#> y ~ x + z
```

Generates five realizations from the model:

``` r
mod$gen(n = 5)
#>            y          x           z          e     .resid    .fitted
#> 1 -2.4082840 -0.9527958  0.28227081 -2.3026985 -2.0724721 -0.3358118
#> 2  1.1162350 -0.5748778  0.36826332  1.3010213  2.5059879 -1.3897529
#> 3 -3.0935405  0.9005904 -0.01223249 -0.3569594 -0.1256719 -2.9678686
#> 4 -6.1790103  0.9489648  0.75171836 -0.6447801 -0.4095148 -5.7694955
#> 5  0.6039986 -0.3168255 -0.31043863  0.6363541  0.1016709  0.5023277
```

**1.3. Make a residual plot**

Method `plot()` produce a `ggplot` object. It takes a data frame
containing columns `.resid` and `.fitted` as input, along with a
character argument type indicating the type of the data plot, and other
aesthetic arguments such as size and alpha to control the appearance of
the plot.

``` r
mod$plot(mod$gen(n = 100), type = "resid", size = 1)
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

**1.4. Make a lineup**

A lineup is a matrix of residual plots which can be produced by using
the methods `gen_lineup()` and `plot_lineup()`. Method `gen_lineup()`
takes the number of realizations `n` and the number of plots in a lineup
`k` as inputs. Other than that, the method `plot_lineup()` has the same
user interface as `plot()`.

Generate a lineup consists of 2 plots and 8 realizations:

``` r
mod$gen_lineup(n = 8, k = 2)
#>             y           x          z          e      .resid    .fitted
#> 1  -0.4698695  0.02360236 -0.2798524 -1.0427085  0.91490332 -1.3847728
#> 2   1.5258921 -0.33129920 -0.4138406  1.7923841  2.16464126 -0.6387492
#> 3  -1.0647215 -0.70393787  0.4286435 -0.5618290 -0.29713409 -0.7675874
#> 4  -3.0386325  0.78990716 -0.6421573 -0.3932404 -0.64758404 -2.3910485
#> 5  -2.3544102 -0.39828558  0.6578589 -1.1751749 -0.83872555 -1.5156846
#> 6  -1.6342183  0.18448867  0.6430988 -1.3022205  0.88867303 -2.5228914
#> 7  -1.7696743 -0.40032344 -0.8246933 -0.8986588 -1.63301478 -0.1366595
#> 8  -3.4891458  0.27877947  0.9118286 -0.6129378 -0.55175915 -2.9373867
#> 9  -2.7192872  0.02360236 -0.2798524 -1.0427085 -1.33451439 -1.3847728
#> 10 -0.5834812 -0.33129920 -0.4138406  1.7923841  0.05526803 -0.6387492
#> 11 -1.6884593 -0.70393787  0.4286435 -0.5618290 -0.92087185 -0.7675874
#> 12 -2.0879371  0.78990716 -0.6421573 -0.3932404  0.30311136 -2.3910485
#> 13 -0.1925118 -0.39828558  0.6578589 -1.1751749  1.32317282 -1.5156846
#> 14 -4.2980259  0.18448867  0.6430988 -1.3022205 -1.77513458 -2.5228914
#> 15  0.8031242 -0.40032344 -0.8246933 -0.8986588  0.93978365 -0.1366595
#> 16 -1.5282017  0.27877947  0.9118286 -0.6129378  1.40918496 -2.9373867
#>    test_name statistic   p_value k  null
#> 1     F-test 0.7670548 0.6827506 1 FALSE
#> 2     F-test 0.7670548 0.6827506 1 FALSE
#> 3     F-test 0.7670548 0.6827506 1 FALSE
#> 4     F-test 0.7670548 0.6827506 1 FALSE
#> 5     F-test 0.7670548 0.6827506 1 FALSE
#> 6     F-test 0.7670548 0.6827506 1 FALSE
#> 7     F-test 0.7670548 0.6827506 1 FALSE
#> 8     F-test 0.7670548 0.6827506 1 FALSE
#> 9     F-test 9.2175269 0.2416036 2  TRUE
#> 10    F-test 9.2175269 0.2416036 2  TRUE
#> 11    F-test 9.2175269 0.2416036 2  TRUE
#> 12    F-test 9.2175269 0.2416036 2  TRUE
#> 13    F-test 9.2175269 0.2416036 2  TRUE
#> 14    F-test 9.2175269 0.2416036 2  TRUE
#> 15    F-test 9.2175269 0.2416036 2  TRUE
#> 16    F-test 9.2175269 0.2416036 2  TRUE
```

Plot a lineup consists of 20 plots and 300 realizations:

``` r
mod$plot_lineup(mod$gen_lineup(n = 300, k = 20), type = "resid", alpha = 0.6)
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="100%" />

## 2. Visual inference heteroskedasticity linear model class `HETER_MODEL`

A visual inference heteroskedasticity linear model is defined as:

``` r
HETER_MODEL$formula
#> y ~ 1 + x + sqrt(1 + (2 - abs(a)) * (x - a)^2 * b) * e
```

where `x` is random variables, `a`, `b` are parameters and `e` is the
random error term.

Similar to the cubic model, the heteroskedasticity model could be built
by the heteroskedasticity model class constructor `heter_model()`. This
function takes three arguments as inputs, which are `a`, `b` and `x`.
`a` and `b` are numeric parameters defined above. `x` needs to be a
random variable object.

``` r
mod <- heter_model(a = 0, b = 16, x = rand_uniform(-1, 1))

mod
#> 
#> ── <HETER_MODEL object>
#> y = 1 + x + sqrt(1 + (2 - abs(a)) * (x - a)^2 * b) * e
#>  - x: <RAND_UNIFORM object>
#>    [a: -1, b: 1]
#>  - e: <RAND_NORMAL object>
#>    [mu: 0, sigma: 1]
#>  - a: 0
#>  - b: 16
```

Since both the cubic model class `CUBIC_MODEL` and the
heteroskedasticity model class `HETER_MODEL` are inherited from the
visual inference model class `VI_MODEL`, heteroskedasticity model object
can be used in a similar way as cubic model object. The following codes
give examples of the use of the object.

``` r
mod$gen(n = 5)
#>            y          x          e     .resid   .fitted
#> 1  2.6882289  0.1028836  1.3701839  1.4093750 1.2788539
#> 2 -0.2705299  0.0545881 -1.2661262 -1.6280278 1.3574978
#> 3  0.9136788  0.4146411 -0.1964683  0.1424887 0.7711902
#> 4  3.3598493 -0.8119117  0.6747753  0.5913495 2.7684998
#> 5  2.0846112 -0.7083106  0.4341517 -0.5151854 2.5997966
```

``` r
mod$plot(mod$gen(n = 300), type = "resid", size = 1)
```

<img src="man/figures/README-unnamed-chunk-13-1.png" width="100%" />

``` r
mod$plot_lineup(mod$gen_lineup(n = 300), alpha = 0.6)
```

<img src="man/figures/README-unnamed-chunk-14-1.png" width="100%" />

## 3. Closed form expression `CLOSED_FORM`

**3.1. Define and evaluate a closed form expression**

“Closed form expression” is defined as a special type of `R` expression
that can replace random variables in the expression with their
realizations before evaluation. This may sound confusing, so let’s look
at some examples.

`closed_form()` is the closed form expression constructor, which takes
only one arguments `expr` required to be a formula. Considering the
simplest case first, where the expression contains zero random
variables, such as `1 + 3`. It can be evaluated using the method
`compute()` directly as a typical `R` expression.

``` r
cf <- closed_form(~1 + 3)
cf
#> 
#> ── <CLOSED_FORM object>
#> EXPR = 1 + 3
cf$compute()
#> [1] 4
```

However, if the closed form expression contains any random variables, it
can not be evaluated directly since the values of random variables are
unknown. In the following example, `x` is a uniform random variable
ranged from -1 to 1, and `x + 1` is an expression that can not be
evaluated by method `compute()`.

``` r
x <- rand_uniform(-1, 1)
cf <- closed_form(~1 + x)
```

In this case, the `gen()` method will try to generate `n` realizations
from random variables, then evaluate the expression. In addition, if
`rhs_val = TRUE`, realizations generated from random variables will be
returned.

``` r
cf$gen(n = 5, rhs_val = TRUE)
#> $lhs
#> [1] 1.3170041 1.7636643 1.6469277 0.5630111 1.5370695
#> 
#> $rhs
#> $rhs$x
#> [1]  0.3170041  0.7636643  0.6469277 -0.4369889  0.5370695
```

**3.2. Nested closed form expression**

Nested closed form expression is useful when a hierarchical structure
present in the expression.

Consider the following statistical model:

`y = x + e`, where `x` is a uniform random variable ranged from -1 to 1,
and `e = 1 + x^2 * z`, where `z` is a standard normal random variable.

We can define this model using the following code:

``` r
z <- rand_normal(mu = 0, sigma = 1)
x <- rand_uniform(-1, 1)
e <- closed_form(~1 + x^2 * z)
y <- closed_form(~x + e)
y
#> 
#> ── <CLOSED_FORM object>
#> EXPR = x + e
#>  - x: <RAND_UNIFORM object>
#>    [a: -1, b: 1]
#>  - e: <CLOSED_FORM object>
#>    EXPR = 1 + x^2 * z
#>     - x: <RAND_UNIFORM object>
#>       [a: -1, b: 1]
#>     - z: <RAND_NORMAL object>
#>       [mu: 0, sigma: 1]
```

Note that `y` depends on `e` and `e` itself is a closed form expression.
Both `y` and `e` depend on `x`. The method `gen()` will return values of
`x`, `z` and `e` if `rhs_val = TRUE`.

``` r
y$gen(n = 5, rhs_val = TRUE)
#> $lhs
#> [1] 1.5498807 1.2346299 0.3426129 1.6373318 2.7634076
#> 
#> $rhs
#> $rhs$x
#> [1] 0.3994252 0.2808786 0.7875256 0.5864617 0.8603465
#> 
#> $rhs$z
#> [1]  0.9430551 -0.5862216 -2.3297659  0.1479049  1.2200295
#> 
#> $rhs$e
#> [1]  1.1504555  0.9537514 -0.4449126  1.0508700  1.9030611
```

**3.3. Turn `gen()` result into dataframe via method `as_dataframe()`**

Results of `gen()` is either a vector or a list, which can be converted
into a dataframe via method `as_dataframe()`. The `lhs` argument is for
naming the left hand side result of the closed form expression.

``` r
y$as_dataframe(y$gen(n = 5, rhs_val = TRUE), lhs = "y")
#>            y          x          z         e
#> 1 1.23631909  0.2953603 -0.6767856 0.9409588
#> 2 0.02214544 -0.5057357 -1.8458824 0.5278812
#> 3 2.33608068  0.9647250  0.3990093 1.3713557
#> 4 1.50697387  0.4625741  0.2075001 1.0443998
#> 5 0.76023852 -0.2661947  0.3730363 1.0264332
```

**3.4. Current limitiation of `CLOSED_FORM`**

For `closed_form()`, any simple expressions can be provided, as long as
all the symbols exist in the current environment. Note that this
function tries to evaluate **ALL** the atomic symbols in the expression
during initialization, and store the values in the object. Hence, calls
like `a$b` will also be decomposed as `$`, `a` and `b`, where `b` will
be interpreted as a variable `b` exists in the current environment.
Therefore, use `~a[["b"]]` instead of `~a$b`. And pre-define function
like `myfun = function() 1`, then use it in the expression `~myfun()`.
