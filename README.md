
<!-- README.md is generated from README.Rmd. Please edit that file -->

# visage

<!-- badges: start -->
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
#>            y           x          z          e      .resid    .fitted
#> 1 -1.9383218 -0.34224546 -0.2037668 -2.0619964 -0.85923527 -1.0790865
#> 2 -2.0571197 -0.78416117 -0.4625270 -1.0673211  0.22900437 -2.2861240
#> 3 -0.8375781 -0.37792106 -0.2851144 -0.7336591  0.41467416 -1.2522523
#> 4  0.5329768  0.03321616  0.5249864  0.2391021 -0.05085478  0.5838316
#> 5  0.8118476  0.16482118  0.2843898 -0.2307978  0.26641152  0.5454361
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
#>              y          x          z          e     .resid     .fitted
#> 1  -4.27650580  0.8099464  0.3040408 -2.4668068 -1.5836131 -2.69289266
#> 2  -3.93085245  0.8232028  0.4570914 -1.5912947 -0.6571641 -3.27368836
#> 3   1.37309891 -0.7475679  0.1727164  1.4761211 -0.3474899  1.72058884
#> 4   1.46668816 -0.3768349  0.3723097  1.3073564  1.3985090  0.06817918
#> 5   0.19696056 -0.9304175  0.4714127  0.8176492 -0.9186596  1.11562019
#> 6  -3.05524501  0.7223357  0.7625360  0.2304052  1.0551359 -4.11038090
#> 7  -3.63808956  0.7617544  0.6630701 -0.8024701  0.2164698 -3.85455938
#> 8   1.17496314  0.3693298 -0.2316412  0.7213079  0.8368121  0.33815103
#> 9  -3.42654104  0.8099464  0.3040408 -2.4668068 -0.7336484 -2.69289266
#> 10 -4.82606447  0.8232028  0.4570914 -1.5912947 -1.5523761 -3.27368836
#> 11  0.55640521 -0.7475679  0.1727164  1.4761211 -1.1641836  1.72058884
#> 12 -0.06342362 -0.3768349  0.3723097  1.3073564 -0.1316028  0.06817918
#> 13  1.85959257 -0.9304175  0.4714127  0.8176492  0.7439724  1.11562019
#> 14 -3.42530762  0.7223357  0.7625360  0.2304052  0.6850733 -4.11038090
#> 15 -2.95117807  0.7617544  0.6630701 -0.8024701  0.9033813 -3.85455938
#> 16  1.58753498  0.3693298 -0.2316412  0.7213079  1.2493839  0.33815103
#>    test_name statistic   p_value k  null
#> 1     F-test  35.49463 0.1251533 2 FALSE
#> 2     F-test  35.49463 0.1251533 2 FALSE
#> 3     F-test  35.49463 0.1251533 2 FALSE
#> 4     F-test  35.49463 0.1251533 2 FALSE
#> 5     F-test  35.49463 0.1251533 2 FALSE
#> 6     F-test  35.49463 0.1251533 2 FALSE
#> 7     F-test  35.49463 0.1251533 2 FALSE
#> 8     F-test  35.49463 0.1251533 2 FALSE
#> 9     F-test   2.41691 0.4449079 1  TRUE
#> 10    F-test   2.41691 0.4449079 1  TRUE
#> 11    F-test   2.41691 0.4449079 1  TRUE
#> 12    F-test   2.41691 0.4449079 1  TRUE
#> 13    F-test   2.41691 0.4449079 1  TRUE
#> 14    F-test   2.41691 0.4449079 1  TRUE
#> 15    F-test   2.41691 0.4449079 1  TRUE
#> 16    F-test   2.41691 0.4449079 1  TRUE
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
#>            y           x          e    .resid    .fitted
#> 1 -4.5334688  0.39166555 -2.4375095 -4.158807 -0.3746620
#> 2  1.6712434  0.22950788  0.2695533  2.370509 -0.6992658
#> 3  0.9231221  0.07416540 -0.1392819  1.933349 -1.0102270
#> 4 -3.7977785 -0.68119120 -1.0340480 -1.275495 -2.5222836
#> 5  0.1002552  0.06419368 -0.9060491  1.130443 -1.0301882
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
#> [1] 0.9780757 1.8840041 1.1278455 0.8508702 0.9230486
#> 
#> $rhs
#> $rhs$x
#> [1] -0.02192433  0.88400405  0.12784547 -0.14912984 -0.07695142
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
#> [1]  0.7593559 -0.1501161  0.9376728  2.2168347  0.7618522
#> 
#> $rhs
#> $rhs$x
#> [1] -0.23491061 -0.72941248 -0.05920525  0.91934543 -0.31375142
#> 
#> $rhs$z
#> [1] -0.1038991 -0.7907330 -0.8906593  0.3519767  0.7680177
#> 
#> $rhs$e
#> [1] 0.9942665 0.5792964 0.9968780 1.2974893 1.0756036
```

**3.3. Turn `gen()` result into dataframe via method `as_dataframe()`**

Results of `gen()` is either a vector or a list, which can be converted
into a dataframe via method `as_dataframe()`. The `lhs` argument is for
naming the left hand side result of the closed form expression.

``` r
y$as_dataframe(y$gen(n = 5, rhs_val = TRUE), lhs = "y")
#>            y           x          z         e
#> 1  1.2127933  0.19282444  0.5370681 1.0199689
#> 2 -0.2652243 -0.88646682 -0.4819881 0.6212425
#> 3  1.0869776 -0.99510024  1.0927601 2.0820778
#> 4  0.3549894 -0.74064549  0.1743394 1.0956349
#> 5  1.0867490  0.08777789 -0.1335390 0.9989711
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
#>  [5] "..new.."          "..repr.."         "has_attr"         "instantiation"   
#>  [9] "set_attr"         "..type.."         "get_attr"         "..methods.."     
#> [13] "..method_env.."   "..instantiated.." "..init.."
DEMO$..methods..()
#>  [1] "..dict.."      "..str.."       "..len.."       "..new.."      
#>  [5] "..repr.."      "has_attr"      "instantiation" "set_attr"     
#>  [9] "get_attr"      "..methods.."   "..init.."
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
