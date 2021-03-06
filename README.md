
<!-- README.md is generated from README.Rmd. Please edit that file -->

# visage

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/TengMCing/visage/branch/master/graph/badge.svg)](https://app.codecov.io/gh/TengMCing/visage?branch=master)
<!-- badges: end -->

The goal of visage is to provide a set of tools for running visual
inference experiments on linear regression models.

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
#>             y          x          z          e     .resid    .fitted
#> 1 -3.58891360  0.9233096 -0.8366259  0.5863286 -1.6018899 -1.9870237
#> 2 -1.95124447 -0.3487659 -0.6727788 -1.2477243 -0.9636916 -0.9875529
#> 3  0.03063862  0.4558134 -0.9777414  0.5238079  2.0571001 -2.0264615
#> 4  0.81797183  0.0770836  0.2033810 -0.2939637  0.2591844  0.5587875
#> 5 -0.19549152  0.7433097 -0.1200783  1.1089237  0.2492970 -0.4447885
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
#>             y          x           z          e       .resid     .fitted
#> 1  -3.3892609  0.8532920 -0.60702697 -0.1529302 -1.404858142 -1.98440280
#> 2  -0.8676630  0.6067390 -0.47159924  0.1242328  0.816597117 -1.68426014
#> 3  -0.1433487 -0.6900618 -0.88785758  1.1425373 -0.567226294  0.42387758
#> 4  -1.0696662  0.4310205  0.28733025 -1.6716168  0.711452072 -1.78111823
#> 5  -3.1407963  0.5884935  0.72864214 -1.0542202 -0.920355373 -2.22044091
#> 6   0.5796974  0.2338755 -0.45816042  0.3476672  1.720268932 -1.14057158
#> 7  -0.3567020 -0.4448938  0.01343071 -0.5950732  0.003896152 -0.36059815
#> 8  -0.4016684 -0.4065016 -0.78659962  0.4817655 -0.359774464 -0.04189396
#> 9  -1.3169887  0.8532920 -0.60702697 -0.1529302  0.667414147 -1.98440280
#> 10 -3.8029193  0.6067390 -0.47159924  0.1242328 -2.118659113 -1.68426014
#> 11  0.5300447 -0.6900618 -0.88785758  1.1425373  0.106167090  0.42387758
#> 12 -1.4719438  0.4310205  0.28733025 -1.6716168  0.309174396 -1.78111823
#> 13 -1.7587193  0.5884935  0.72864214 -1.0542202  0.461721592 -2.22044091
#> 14 -0.2338350  0.2338755 -0.45816042  0.3476672  0.906736618 -1.14057158
#> 15 -1.3245366 -0.4448938  0.01343071 -0.5950732 -0.963938396 -0.36059815
#> 16  0.5894897 -0.4065016 -0.78659962  0.4817655  0.631383667 -0.04189396
#>    test_name   statistic    p_value k  null
#> 1     F-test 173.2354797 0.05691425 1 FALSE
#> 2     F-test 173.2354797 0.05691425 1 FALSE
#> 3     F-test 173.2354797 0.05691425 1 FALSE
#> 4     F-test 173.2354797 0.05691425 1 FALSE
#> 5     F-test 173.2354797 0.05691425 1 FALSE
#> 6     F-test 173.2354797 0.05691425 1 FALSE
#> 7     F-test 173.2354797 0.05691425 1 FALSE
#> 8     F-test 173.2354797 0.05691425 1 FALSE
#> 9     F-test   0.2171396 0.90157799 2  TRUE
#> 10    F-test   0.2171396 0.90157799 2  TRUE
#> 11    F-test   0.2171396 0.90157799 2  TRUE
#> 12    F-test   0.2171396 0.90157799 2  TRUE
#> 13    F-test   0.2171396 0.90157799 2  TRUE
#> 14    F-test   0.2171396 0.90157799 2  TRUE
#> 15    F-test   0.2171396 0.90157799 2  TRUE
#> 16    F-test   0.2171396 0.90157799 2  TRUE
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
#>           y          x          e     .resid    .fitted
#> 1 -2.170323 -0.5588948 -0.7875317 -1.7200943 -0.4502287
#> 2  2.984585 -0.2976523  1.1653928  2.8118906  0.1726940
#> 3 -1.419047  0.2537678 -1.5277621 -2.9065811  1.4875339
#> 4  3.383216  0.4000468  0.8015702  1.5468856  1.8363306
#> 5  1.100347 -0.0209633  0.1204666  0.2678992  0.8324482
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
#> [1] 0.9750361 1.0564543 0.1451794 1.4717265 1.5904042
#> 
#> $rhs
#> $rhs$x
#> [1] -0.02496395  0.05645430 -0.85482059  0.47172645  0.59040424
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
#> [1]  1.2412541  0.7436468 -0.4735227  0.8451914  0.3921465
#> 
#> $rhs
#> $rhs$x
#> [1] -0.9925114 -0.2085894 -0.8250557 -0.1542481 -0.8308623
#> 
#> $rhs$z
#> [1]  1.25245355 -1.09777801 -0.95262380 -0.02355615  0.32304569
#> 
#> $rhs$e
#> [1] 2.2337655 0.9522362 0.3515329 0.9994395 1.2230088
```

**3.3. Turn `gen()` result into dataframe via method `as_dataframe()`**

Results of `gen()` is either a vector or a list, which can be converted
into a dataframe via method `as_dataframe()`. The `lhs` argument is for
naming the left hand side result of the closed form expression.

``` r
y$as_dataframe(y$gen(n = 5, rhs_val = TRUE), lhs = "y")
#>             y          x            z         e
#> 1 -0.22628761 -0.6669281 -1.257572518 0.4406405
#> 2  1.02807155  0.7689187 -1.253048259 0.2591529
#> 3  0.04497571 -0.9471222 -0.008809068 0.9920979
#> 4  1.21070276  0.2920914 -0.953951360 0.9186114
#> 5  1.30983252  0.2867094  0.281295600 1.0231231
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
