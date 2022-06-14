
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
4.  [`visage` OOP system](#4-visage-oop-system)

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
#>            y           x           z          e     .resid    .fitted
#> 1  0.0719453 -0.31596582 -0.77403850  0.7729648 -0.2567667  0.3287120
#> 2 -5.7677403  0.85144886  0.70339282 -1.7675549 -1.6314990 -4.1362414
#> 3 -3.0681272  0.80772202  0.71014817  0.5394933  1.2848012 -4.3529285
#> 4  0.4090330  0.07978173 -0.56331019  0.3288894 -0.1998464  0.6088793
#> 5 -0.3787237  0.35626343 -0.09477948 -1.0993868  0.8033109 -1.1820345
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
#>             y          x          z          e     .resid    .fitted test_name
#> 1  -3.2615820  0.6106806 -0.2156965 -2.7450799 -1.7195850 -1.5419970    F-test
#> 2   0.7038132  0.4477366  0.3617162  0.2995304  1.9679069 -1.2640937    F-test
#> 3  -3.5795666 -0.9877663  0.6954863 -1.7911519 -1.2349343 -2.3446323    F-test
#> 4  -1.2941408 -0.3259794  0.5576815 -0.8576162  0.5404190 -1.8345597    F-test
#> 5  -0.2020867 -0.3725803  0.3001553 -0.5169683  1.8667876 -2.0688743    F-test
#> 6  -1.4315590  0.3352395  0.8140151  0.4753818 -0.3991623 -1.0323967    F-test
#> 7  -2.2560343 -0.2757001  0.6306354 -1.5002905 -0.5221626 -1.7338717    F-test
#> 8  -1.4540245  0.3790839  0.8639466  1.0712568 -0.4992692 -0.9547553    F-test
#> 9  -0.5241608  0.6106806 -0.2156965 -2.7450799  1.0178362 -1.5419970    F-test
#> 10 -3.2701586  0.4477366  0.3617162  0.2995304 -2.0060649 -1.2640937    F-test
#> 11 -1.5911634 -0.9877663  0.6954863 -1.7911519  0.7534689 -2.3446323    F-test
#> 12 -2.0215153 -0.3259794  0.5576815 -0.8576162 -0.1869555 -1.8345597    F-test
#> 13 -1.6169754 -0.3725803  0.3001553 -0.5169683  0.4518990 -2.0688743    F-test
#> 14 -1.3161049  0.3352395  0.8140151  0.4753818 -0.2837082 -1.0323967    F-test
#> 15 -3.4555007 -0.2757001  0.6306354 -1.5002905 -1.7216290 -1.7338717    F-test
#> 16  1.0203983  0.3790839  0.8639466  1.0712568  1.9751536 -0.9547553    F-test
#>    statistic   p_value k  null
#> 1  14.888465 0.1917004 2 FALSE
#> 2  14.888465 0.1917004 2 FALSE
#> 3  14.888465 0.1917004 2 FALSE
#> 4  14.888465 0.1917004 2 FALSE
#> 5  14.888465 0.1917004 2 FALSE
#> 6  14.888465 0.1917004 2 FALSE
#> 7  14.888465 0.1917004 2 FALSE
#> 8  14.888465 0.1917004 2 FALSE
#> 9   3.535829 0.3769765 1  TRUE
#> 10  3.535829 0.3769765 1  TRUE
#> 11  3.535829 0.3769765 1  TRUE
#> 12  3.535829 0.3769765 1  TRUE
#> 13  3.535829 0.3769765 1  TRUE
#> 14  3.535829 0.3769765 1  TRUE
#> 15  3.535829 0.3769765 1  TRUE
#> 16  3.535829 0.3769765 1  TRUE
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
#>            y           x          e     .resid   .fitted
#> 1  1.9458094 -0.18762901  0.7772492 -1.7495400 3.6953494
#> 2 10.4860777 -0.84373883  2.1182674  0.8269328 9.6591448
#> 3  1.9929016  0.12781249  0.7010453  1.1647987 0.8281029
#> 4  0.6477066 -0.03684373 -0.3088137 -1.6770604 2.3247670
#> 5  2.4188607  0.11066230  1.1088511  1.4348689 0.9839918
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
#> [1] 0.4249562 1.9151282 0.5706812 1.2888759 1.3772548
#> 
#> $rhs
#> $rhs$x
#> [1] -0.5750438  0.9151282 -0.4293188  0.2888759  0.3772548
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
#> [1] 0.6958777 0.5871584 0.6032451 0.7325184 1.0207915
#> 
#> $rhs
#> $rhs$x
#> [1] -0.43188726 -0.51129573 -0.30614025 -0.88364633  0.02060055
#> 
#> $rhs$z
#> [1]  0.6849689  0.3766080 -0.9668462  0.7891142  0.4500578
#> 
#> $rhs$e
#> [1] 1.1277649 1.0984541 0.9093854 1.6161647 1.0001910
```

**3.3. Turn `gen()` result into dataframe via method `as_dataframe()`**

Results of `gen()` is either a vector or a list, which can be converted
into a dataframe via method `as_dataframe()`. The `lhs` argument is for
naming the left hand side result of the closed form expression.

``` r
y$as_dataframe(y$gen(n = 5, rhs_val = TRUE), lhs = "y")
#>          y           x          z         e
#> 1 1.216533  0.31454206 -0.9906224 0.9019911
#> 2 1.094176  0.08841144  0.7374344 1.0057642
#> 3 0.870290 -0.71465658  1.1453056 1.5849466
#> 4 1.197807  0.18436565  0.3954489 1.0134416
#> 5 1.377490  0.36336589  0.1069708 1.0141239
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

## 4. `visage` OOP system

**4.1. Define a new class**

A class can be defined with the `new_class` function. All positional
arguments are for specifying parent classes, `BASE` is the base object
class provided by the package, you don’t need to manually specify it.
But if you would like to have advanced behaviour, you can try to
implement your own `object` class.

Class name is mandatory and should be unique.

``` r
# You don't actually need to specify BASE here. This is only for demonstration.
DEMO <- new_class(BASE, class_name = "DEMO")
DEMO
#> 
#> ── <DEMO class>
```

The object is an environment containing some useful attributes and
methods.

-   `OBJECT$..type..` gives the current class name.
-   `OBJECT$..class..` gives the current class name and parent class
    names.

``` r
DEMO$..type..
#> [1] "DEMO"
DEMO$..class..
#> [1] "DEMO" "BASE"
```

-   `OBJECT$..dict..()` returns all names of attribute and method of the
    object.
-   `OBJECT$..methods..()` returns all names of method of the object

``` r
DEMO$..dict..()
#>  [1] "..dict.."         "..str.."          "..len.."          "..class.."       
#>  [5] "..new.."          "..repr.."         "del_attr"         "has_attr"        
#>  [9] "instantiation"    "set_attr"         "..type.."         "get_attr"        
#> [13] "..methods.."      "..method_env.."   "..instantiated.." "..init.."
DEMO$..methods..()
#>  [1] "..dict.."      "..str.."       "..len.."       "..new.."      
#>  [5] "..repr.."      "del_attr"      "has_attr"      "instantiation"
#>  [9] "set_attr"      "get_attr"      "..methods.."   "..init.."
```

-   `OBJECT$..str..()` returns a string representation of the object,
    which will be used by the S3 `print()` method. This method usually
    needs to be overridden in subclass to give short summary of the
    object.

``` r
DEMO$..str..()
#> [1] "<DEMO class>"
```

**4.2. Register a method for the class**

Methods can be registered by using `register_method()`. The first
argument is the object you want to bind the function to, the rest of the
positional arguments are for specifying method names and functions. The
syntax is `method_name = function`.

You can choose to write inline function or pass pre-defined function.
The associative environment of the function doesn’t matter, it will be
modified by the `register_method()` function.

``` r
pre_defined_fn <- function() 1 + 2

register_method(DEMO, inline_fn = function() 1 + 1, pre_defined_fn = pre_defined_fn)

DEMO$inline_fn()
#> [1] 2
DEMO$pre_defined_fn()
#> [1] 3
```

For method that needs to access the object itself, just simply use
`self` in your method. It is an reference to the object.

``` r
DEMO$val <- 5

register_method(DEMO, get_val = function() self$val)

DEMO$get_val()
#> [1] 5
```

**4.3. Override the `..init..()` method**

`..init..()` method is for instance initialization. To override the
`..init..()` method, you need to use the `register_method()` to register
it again.

``` r
init <- function(first_name, employee_id) {
  self$first_name <- first_name
  self$employee_id <- employee_id
}

register_method(DEMO, ..init.. = init)
```

Now the class requires two two arguments `first_name` and `employee_id`
to initialize the instance.

**4.4. Build an instance**

To new and initialize an instance, you need to use the `instantiation()`
method. The output will show it is an object.

``` r
mike <- DEMO$instantiation("Mike", 25)
mike
#> 
#> ── <DEMO object>
```

`first_name` and `employee_id` are stored in the object because of the
`..init..()` method.

``` r
mike$first_name
#> [1] "Mike"
mike$employee_id
#> [1] 25
```

**4.5. A complete workflow**

It is recommend to write your class definition in a function to make
debugging easier. The following example new a class `DEMO_2`, defines
its own `..init..()` method, defines a `get_email()` function for
retrieving the email address, defines its own `..str..()` method such
that when we print the object, it will provide us with a nicely
formatted summary.

`use_method` is used to run methods from other classes, which in this
case, the `..str..()` method from the `BASE` class.

``` r
class_DEMO_2 <- function(env = new.env(parent = parent.frame())) {
  
  new_class(env = env, class_name = "DEMO_2")
  
  init_ <- function(first_name, employee_id) {
    self$first_name <- first_name
    self$employee_id <- employee_id
  }
  
  get_email_ <- function() {
    paste0(self$first_name, "_", self$employee_id, "@company.com")
  }
  
  str_ <- function() {
    paste(use_method(self, BASE$..str..)(), 
          paste("Name:", self$first_name,
                "\nEmployee ID:", self$employee_id,
                "\nEmail:", self$get_email()), 
          sep = "\n")
  }
  
  register_method(env,
                  ..init.. = init_,
                  get_email = get_email_,
                  ..str.. = str_)
  
  return(env)
}
```

``` r
DEMO_2 <- class_DEMO_2()
mike <- DEMO_2$instantiation("Mike", 25)
mike$get_email()
#> [1] "Mike_25@company.com"
```

``` r
mike$..str..()
#> [1] "<DEMO_2 object>\nName: Mike \nEmployee ID: 25 \nEmail: Mike_25@company.com"
mike
#> 
#> ── <DEMO_2 object>
#> Name: Mike 
#> Employee ID: 25 
#> Email: Mike_25@company.com
```
