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
