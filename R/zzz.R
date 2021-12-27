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
#'
#' # Inherit from BASE
#' TEST <- new_class(BASE, class_name = "TEST")
#' TEST$..type..
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
#'
#' # Inherit from BASE
#' TEST <- new_class(BASE, class_name = "TEST")
#' TEST$..class..
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
#'
#' # Inherit from BASE
#' TEST <- new_class(BASE, class_name = "TEST")
#' TEST$..method_env..
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
#'
#' # Instantiation
#' test <- BASE$instantiation()
#' test$..instantiated..
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
#'
#' # Instantiation
#' test <- BASE$instantiation()
#' test$..dict..()
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
#'
#' # Inherit from BASE
#' TEST <- new_class(BASE, class_name = "TEST")
#' TEST$..str..()
#'
#' # Instantiation
#' test <- BASE$instantiation()
#' test$..str..()
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
#'
#' # Inherit from BASE
#' TEST <- new_class(BASE, class_name = "TEST")
#'
#' # Override the `..len..` method
#' register_method(TEST, ..len.. = function() 1)
#' TEST$..len..()
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

#' Get value of an attribute or a method
#'
#' @name BASE$get_attr
#'
#' @description This function gets the value of an attribute or a method.
#' @param attr_name Character. Attribute name.
#' @return The attribute value.
#'
#' @examples
#'
#' BASE$get_attr("test")
#'
#' BASE$get_attr("..methods..")
BASE$get_attr


#' Set value of an attribute or a method
#'
#' @name BASE$set_attr
#'
#' @description This function sets the value of an attribute or a method.
#' @param attr_name Character. Attribute name.
#' @param attr_val Any value.
#' @return No return value, called for side effects.
#'
#' @examples
#'
#' BASE$set_attr("test", 1)
#'
#' BASE$test
BASE$set_attr

#' List all methods of a class or an instance
#'
#' @name BASE$..methods..
#'
#' @description This function lists all methods of a class or an instance
#' @return A string vector.
#'
#' @examples
#'
#' BASE$..methods..()
BASE$..methods..

#' Initialization method
#'
#' @name BASE$..init..
#'
#' @description This function will be called after an instance is built. User
#' could override this function in derived class.
#' @param ... Ignored by `BASE`, but user can define their owns.
#' @return No return value, called for side effects.
#'
#' @examples
#'
#' BASE$..init..
#'
#' # Inherit from BASE
#' TEST <- new_class(BASE, class_name = "TEST")
#'
#' # Override the `..init..` method
#' register_method(TEST, ..init.. = function(a) {self$x <- a})
#'
#' # Build a `TEST` instance
#' test <- TEST$instantiation(a = 2)
#'
#' test$x
BASE$..init..

#' Build a new instance from a class or an instance
#'
#' @name BASE$..new..
#'
#' @description This function will copy all methods and attributes, except
#' the container, and the instantiation method. Then, the `..init_call..` attribute
#' will be set to the current system call, and the `..instantiated..` attribute
#' will be set to `TRUE`. Notice, the `..init..` method will not run.
#' @param env Environment. The instance environment.
#' @param init_call Call. Contents of the `..init_call..`. It is recommended to
#' use the default value.
#' @return An instance environment.
#'
#' @examples
#'
#' BASE$..new..()
#'
#' # Inherit from BASE
#' TEST <- new_class(BASE, class_name = "TEST")
#'
#' TEST$..new..()
BASE$..new..


#' Instantiation method
#'
#' @name BASE$instantiation
#'
#' @description This function will new an instance using the `..new..` method,
#' then initialized the instance with the `..init..` method.
#' @param ... Arguments passed to `..init..` method.
#' @param env Environment. The instance environment.
#' @return An instance environment.
#'
#' @examples
#'
#' BASE$..dict..()
#'
#' # Build an instance
#' base_instance <- BASE$instantiation()
#'
#' base_instance$..dict..()
BASE$instantiation
