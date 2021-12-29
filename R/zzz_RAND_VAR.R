
# RAND_VAR ----------------------------------------------------------------



#' RAND_VAR class environment
#'
#' @name RAND_VAR
#'
#' @description This is the base class of random variable, inherited from
#' [BASE].
#' @format An environment with S3 class `oop`.
#' @seealso Parent class: [BASE]
#' \cr
#' \cr
#' New methods: [RAND_VAR$..init..], [RAND_VAR$..str..],
#' [RAND_VAR$E], [RAND_VAR$Var]
#' @export
RAND_VAR <- class_RAND_VAR()

#' Initialization method
#'
#' @name RAND_VAR$..init..
#'
#' @description This function will be called after an instance is built. User
#' input will be stored in the environment.
#' @param dist Character. Distribution name.
#' @param prm List. List of parameters.
#' @return No return value, called for side effects.
#'
#' @examples
#'
#' RAND_VAR$..init..
#'
#' # Instantiation
#' test <- RAND_VAR$instantiation(dist = "uniform", prm = list(a = 1, b = 2))
#' test$prm
#' test$dist
RAND_VAR$..init..

#' Expectation of the random variable
#'
#' @name RAND_VAR$E
#'
#' @description Expectation of the random variable.
#' User could override this method in derived class.
#' @return NA
#'
#' @examples
#'
#' RAND_VAR$E()
RAND_VAR$E

#' Variance of the random variable
#'
#' @name RAND_VAR$Var
#'
#' @description Variance of the random variable.
#' User could override this method in derived class.
#' @return NA
#'
#' @examples
#'
#' RAND_VAR$Var()
RAND_VAR$Var

#' Generate random values
#'
#' @name RAND_VAR$gen
#'
#' @description This function generates random values from the random variable.
#' @param n Integer. Number of observations.
#' @return NA. User needs to define their own `gen` method in derived class.
#'
#' @examples
#'
#' test <- RAND_VAR$gen(10)
RAND_VAR$gen

#' String representation of the object
#'
#' @name RAND_VAR$..str..
#'
#' @description This function returns a string representation of the object.
#' @return A string.
#'
#' @examples
#'
#' RAND_VAR$..str..()
#'
#' test <- RAND_VAR$instantiation(dist = "uniform", prm = list(a = 1, b = 2))
#' test$..str..()
RAND_VAR$..str..



# RAND_UNIFORM ------------------------------------------------------------

#' RAND_UNIFORM class environment
#'
#' @name RAND_UNIFORM
#'
#' @description This is the class of the uniform random variable, inherited from
#' [RAND_VAR].
#' @format An environment with S3 class `oop`.
#' @seealso Parent class: [RAND_VAR]
#' \cr
#' \cr
#' New methods: [RAND_UNIFORM$..init..], [RAND_UNIFORM$gen],
#' RAND_UNIFORM$E, RAND_UNIFORM$Var
#' @export
RAND_UNIFORM <- class_RAND_UNIFORM()

#' Initialization method
#'
#' @name RAND_UNIFORM$..init..
#'
#' @description This function will be called after an instance is built. User
#' input will be stored in the environment.
#' @param a Numeric.
#' @param b Numeric.
#' @return No return value, called for side effects.
#'
#' @examples
#'
#' # Instantiation
#' test <- RAND_UNIFORM$instantiation(a = 1, b = 2)
#' test
RAND_UNIFORM$..init..

#' Generate random values
#'
#' @name RAND_UNIFORM$gen
#'
#' @description This function generates random values from the random variable.
#' @param n Integer. Number of observations.
#' @return A vector of numeric values.
#' @seealso [stats::runif()]
#'
#' @examples
#'
#' test <- RAND_UNIFORM$instantiation(a = 1, b = 2)
#' test$gen(10)
RAND_UNIFORM$gen



# RAND_NORMAL -------------------------------------------------------------

#' RAND_NORMAL class environment
#'
#' @name RAND_NORMAL
#'
#' @description This is the class of the normal random variable, inherited from
#' [RAND_VAR].
#' @format An environment with S3 class `oop`.
#' @seealso Parent class: [RAND_VAR]
#' \cr
#' \cr
#' New methods: [RAND_NORMAL$..init..], [RAND_NORMAL$gen],
#' RAND_NORMAL$E, RAND_NORMAL$Var
#' @export
RAND_NORMAL <- class_RAND_NORMAL()

#' Initialization method
#'
#' @name RAND_NORMAL$..init..
#'
#' @description This function will be called after an instance is built. User
#' input will be stored in the environment.
#' @param mu Numeric.
#' @param sigma Numeric.
#' @return No return value, called for side effects.
#'
#' @examples
#'
#' # Instantiation
#' test <- RAND_NORMAL$instantiation(mu = 1, sigma = 2)
#' test
RAND_NORMAL$..init..

#' Generate random values
#'
#' @name RAND_NORMAL$gen
#'
#' @description This function generates random values from the random variable.
#' @param n Integer. Number of observations.
#' @return A vector of numeric values.
#' @seealso [stats::rnorm()]
#'
#' @examples
#'
#' test <- RAND_NORMAL$instantiation(mu = 1, sigma = 2)
#' test$gen(10)
RAND_NORMAL$gen
