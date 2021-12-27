#' BASE class environment
#'
#' @name BASE
#'
#' @description This class provides essential attributes and methods.
#' @format An environment with S3 class `oop`.
#' @export
BASE <- class_BASE()

#' Class name
#'
#' @name BASE$..type..
#'
#' @description A string.
#'
#' @examples
#'
#' BASE$..type..
BASE$..type..

#' Class name and parent class names
#'
#' @name BASE$..class..
#'
#' @description A string vector.
#'
#' @examples
#'
#' BASE$..class..
BASE$..class..

#' The container
#'
#' @name BASE$..method_env..
#'
#' @description A container where methods will be executed.
#'
#' @examples
#'
#' BASE$..method_env..
BASE$..method_env..

#' Instantiation status
#'
#' @name BASE$..instantiated..
#'
#' @description Whether or not the object is an instance.
#'
#' @examples
#'
#' BASE$..instantiated..
BASE$..instantiated..

#' All names in the class or instance environment
#'
#' @name BASE$..dict..
#'
#' @description This function returns all names in the environment.
#' @return A vector of string.
#'
#' @examples
#'
#' BASE$..dict..()
BASE$..dict..

#' String representation of the object
#'
#' @name BASE$..str..
#'
#' @description This function returns a string representation of the object.
#' @return A string.
#'
#' @examples
#'
#' BASE$..str..()
BASE$..str..

#' Length of the class or the instance
#'
#' @name BASE$..len..
#'
#' @description User could override this method in derived class.
#'
#' @examples
#'
#' BASE$..len..()
BASE$..len..

#' Whether or not an attribute or method exists
#'
#' @name BASE$has_attr
#'
#' @description This function checks whether or not an attribute or method
#' exists.
#' @param attr_name Character. Attribute name.
#' @return True or FALSE.
#'
#' @examples
#'
#' BASE$has_attr("test")
#'
#' BASE$has_attr("..len..")
BASE$has_attr
