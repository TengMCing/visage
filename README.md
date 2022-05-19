
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

## Basic examples for usgae of `visage` OOP system

``` r
library(visage)
```

### Define a new class

A class can be defined with the `new_class` function. All positional
arguments are for specifying parent classes, `BASE` is the base object
class provided by the package, so you almost always need to include it
in . If you would like to have advanced behaviour, you can try to
implement your own `object` class.

Class name is mandatory.

``` r
DEMO <- new_class(BASE, class_name = "DEMO")
DEMO
#> 
#> ── <DEMO class>
```
