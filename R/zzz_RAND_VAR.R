#' RAND_VAR class environment
#'
#' @name RAND_VAR
#'
#' @description This is the base class of random variable, inherited from
#' [BASE].
#' @format An environment with S3 class `oop`.
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
