
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
# install.packages("remotes")
remotes::install_url("https://github.com/TengMCing/visage/raw/master/built/visage_0.1.1.tar.gz")
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
#> Parameters:
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
#>            y            x          z          e     .resid    .fitted
#> 1 -1.4823094 -0.381315470  0.7835510  0.6703734  1.4061902 -2.8884996
#> 2  0.7120971 -0.002402515 -0.6769199  0.8355626  1.2155014 -0.5034043
#> 3  0.6104170  0.336467392 -0.9973950  0.7330245 -1.1286155  1.7390325
#> 4 -5.7280040 -0.531081537  0.9129219 -1.9302331 -1.8470815 -3.8809225
#> 5 -1.1488316 -0.175382074  0.7953012  0.7139479  0.3540055 -1.5028370
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
#>             y          x          z           e     .resid    .fitted test_name
#> 1  -4.2741053  0.2173978  0.9730634 -0.68735752 -1.2616429 -3.0124624    F-test
#> 2   0.3409917  0.3259869  0.1603458 -0.63311304  2.7012045 -2.3602128    F-test
#> 3  -2.2637457  0.6595516 -0.0492654 -1.70135289  0.4888987 -2.7526444    F-test
#> 4  -1.8939956  0.7051014 -0.6626419  0.05110670  0.2995834 -2.1935790    F-test
#> 5  -2.2726905  0.6581385 -0.7364619 -0.61090733 -0.2426241 -2.0300664    F-test
#> 6  -1.0466662 -0.8312880  0.2628276 -0.86674354 -0.7025174 -0.3441488    F-test
#> 7  -1.2840775 -0.5223657  0.8456534  1.74326999  0.2375345 -1.5216119    F-test
#> 8  -4.2859657  0.9472303 -0.5407787 -0.04810181 -1.5204367 -2.7655289    F-test
#> 9  -3.2561988  0.2173978  0.9730634 -0.68735752 -0.2437364 -3.0124624    F-test
#> 10 -2.4902247  0.3259869  0.1603458 -0.63311304 -0.1300119 -2.3602128    F-test
#> 11 -2.4092319  0.6595516 -0.0492654 -1.70135289  0.3434125 -2.7526444    F-test
#> 12 -4.8687212  0.7051014 -0.6626419  0.05110670 -2.6751422 -2.1935790    F-test
#> 13 -1.6497602  0.6581385 -0.7364619 -0.61090733  0.3803062 -2.0300664    F-test
#> 14  0.3631987 -0.8312880  0.2628276 -0.86674354  0.7073476 -0.3441488    F-test
#> 15 -1.9029286 -0.5223657  0.8456534  1.74326999 -0.3813167 -1.5216119    F-test
#> 16 -0.7663880  0.9472303 -0.5407787 -0.04810181  1.9991409 -2.7655289    F-test
#>     statistic    p_value k  null
#> 1  1445.40416 0.01972441 2 FALSE
#> 2  1445.40416 0.01972441 2 FALSE
#> 3  1445.40416 0.01972441 2 FALSE
#> 4  1445.40416 0.01972441 2 FALSE
#> 5  1445.40416 0.01972441 2 FALSE
#> 6  1445.40416 0.01972441 2 FALSE
#> 7  1445.40416 0.01972441 2 FALSE
#> 8  1445.40416 0.01972441 2 FALSE
#> 9    20.24929 0.16497676 1  TRUE
#> 10   20.24929 0.16497676 1  TRUE
#> 11   20.24929 0.16497676 1  TRUE
#> 12   20.24929 0.16497676 1  TRUE
#> 13   20.24929 0.16497676 1  TRUE
#> 14   20.24929 0.16497676 1  TRUE
#> 15   20.24929 0.16497676 1  TRUE
#> 16   20.24929 0.16497676 1  TRUE
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
#> Parameters:
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
#>            y          x           e     .resid    .fitted
#> 1 -1.7982441 -0.7046938 -0.50939638 -2.5840225  0.7857784
#> 2  7.4223277 -0.7233512  1.69637874  6.6008776  0.8214500
#> 3 -1.9015359  0.3384022 -1.50014613 -0.6929879 -1.2085480
#> 4 -0.8498885 -0.9935263 -0.15001515 -2.1878943  1.3380058
#> 5  0.0101072 -0.8931431 -0.01878493 -1.1359729  1.1460801
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
#> [1] 1.1733333 1.4757701 1.1004389 1.0628132 0.2765408
#> 
#> $rhs
#> $rhs$x
#> [1]  0.17333326  0.47577013  0.10043886  0.06281325 -0.72345923
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
#> [1] 1.3171537 0.1137230 1.2278964 1.1174784 0.7516698
#> 
#> $rhs
#> $rhs$x
#> [1]  0.5882566 -0.8464125  0.1883570  0.4431485 -0.3737404
#> 
#> $rhs$z
#> [1] -0.78343075 -0.05564446  1.11446630 -1.65836192  0.89782735
#> 
#> $rhs$e
#> [1] 0.7288970 0.9601355 1.0395394 0.6743300 1.1254102
```

**3.3. Turn `gen()` result into dataframe via method `as_dataframe()`**

Results of `gen()` is either a vector or a list, which can be converted
into a dataframe via method `as_dataframe()`. The `lhs` argument is for
naming the left hand side result of the closed form expression.

``` r
y$as_dataframe(y$gen(n = 5, rhs_val = TRUE), lhs = "y")
#>           y           x          z           e
#> 1 0.4047076 -0.42104891 -0.9828596  0.82575651
#> 2 0.9381029 -0.06229928  0.1036231  1.00040218
#> 3 0.8623083 -0.51855076  1.4163862  1.38085902
#> 4 0.7050461  0.73734006 -1.8987498 -0.03229397
#> 5 0.8528809 -0.98482490  0.8637210  1.83770580
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
