# register_method ---------------------------------------------------------

#' Register method for an instance
#'
#' This function register a function as a method of an instance environment.
#'
#' Methods will be executed inside a container, which is a child
#' environment of the parent of the instance environment. Thus, methods can not
#' access variables of the instance environment directly, but can access
#' variables of the parent of the instance environment directly. The designed
#' way for methods to access the instance environment is by using the name
#' "self", this name can be changed by specifying a string in `self_name`.
#' The default name of the container is "method_env_". This also can be changed
#' by specifying a string in `container_name`. An instance can have multiple
#' containers, but every container is recommended to contain only one self
#' reference.
#' \cr
#' \cr
#' Method needs to be provided as `a = function() 1`, where `a` is the name of
#' the method and the right hand side of the equal sign is the function.
#' Warning will be raised if the container contains contents other than the
#' self reference.
#'
#' @param env Environment. Instance environment.
#' @param ... Named Functions. Functions needs to be provided in named format,
#' like `a = function() 1`.
#' @param container_name Character. Name of the container. Methods will be
#' executed inside this container.
#' @param self_name Character. Name of the self reference. Methods needs to use
#' this name to access the instance environment.
#' @return No return value, called for side effects.
#'
#' @examples
#'
#' a <- function() print(self$x)
#'
#' e <- new.env()
#' e$x <- 1
#' register_method(e, aa = a, self_name = "self")
#'
#' e$aa()
#'
#' @export

register_method <- function(env, ..., container_name = "method_env_", self_name = "self") {

  # Capture function call
  fn_list <- as.list(sys.call())

  # `env` must be provided
  if (!is.environment(env)) stop("`env` is not an environment!")

  # `container_name` and `self_name` must be provided as characters
  if (!is.character(container_name)) stop("`container_name` must be characters!")
  if (!is.character(self_name)) stop("`self_name` must be characters!")

  # Check if container exists
  if (container_name %in% names(env)) {

    # Check if the container is an environment
    if (!is.environment(env[[container_name]])) stop(container_name, " exists, but it is not an environment! Consider remove it.")

    # Check if the container is empty (except self)
    if (sum(names(env[[container_name]]) != self_name) > 0) warning("The container is not empty!")

    # Check if the container is the child of the parent of the instance environment
    if (!identical(parent.env(env[[container_name]]), parent.env(env))) stop(container_name, " exists, but it is not a child of the parent of the instance environment! Consider remove it.")

    # Check if self exists
    if (self_name %in% names(env[[container_name]])) {

      # Check if self point to env
      if (!identical(env[[container_name]][[self_name]], env)) stop(self_name, " exists, but it is not the same as the provided environment! Consider remove it.")

    } else {

      # Otherwise, create self
      env[[container_name]][[self_name]] <- env
    }

  } else {

    # Otherwise, create the container and self
    env[[container_name]] <- new.env(parent = parent.env(env))
    env[[container_name]][[self_name]] <- env
  }

  # Extract `...`, check how `env` is provided
  if (is.null(fn_list$env)) {
    fn_list[1:2] <- NULL
  } else {
    fn_list[[1]] <- NULL
    fn_list$env <- NULL
  }

  fn_list$container_name <- NULL
  fn_list$self_name <- NULL

  # Check if `fn_names` are provided via named arguments and delete all "`"
  # A unnamed list will return NULL, a named list but without names will return empty strings
  fn_names <- names(fn_list)

  if (is.null(fn_names) || "" %in% fn_names) stop("All methods should have a name.")
  fn_names <- gsub('`', '', fn_names)

  for (i in 1:length(fn_list)) {

    # Eval the function in the parent frame
    eval_fn <- eval(fn_list[[i]], envir = parent.frame())

    # Check whether it is a function
    if (!is.function(eval_fn)) stop("`", as.expression(fn_list[[i]]), "` is not a function!")

    # Bind it to the target environment
    env[[fn_names[i]]] <- eval_fn
    bind_fn_2_env(env[[container_name]], env[[fn_names[i]]])
  }

  return(invisible(NULL))
}


# register_class_ctor -----------------------------------------------------

#' Register class constructor
#'
#' This function save the class information in the `pseudo_oop_class` attribute
#' of the class constructor. The registration affects the functionality of
#' [is_subclass()] and [is_instance()].
#'
#' This function needs to be called followed by the definition of the class
#' constructor.
#'
#' @param cls Function. The class constructor.
#' @param cls_name Character. Class name of the class constructor.
#' @param parent Function. The parent class constructor.
#' @param ... ignored.
#' @return No return value, called for side effects.
#'
#' @examples
#'
#' # Define a derived class constructor
#' myclass <- function(..., env = new.env(parent = parent.frame())) {
#'   env <- inherit(env, BASE, "myclass", ...)
#'
#'   myfunc_ <- function() 1 + 1
#'
#'   register_method(env, myfunc = myfunc_)
#'   return(env)
#' }
#'
#' register_class_ctor(myclass, "myclass", parent = BASE)
#' @export

register_class_ctor <- function(cls, cls_name, parent = NULL, ...) {
  eval(substitute(attr(cls, "pseudo_oop_class") <- c(cls_name, attr(parent, "pseudo_oop_class"))),
       envir = parent.frame())
}


# class_method ------------------------------------------------------------

#' Get the class method
#'
#' This function construct an instance by using the provided class constructor,
#' then get the method from the instance and set its evaluation environment to
#' the container of the provided instance environment.
#'
#' If the body of the method depends on the arguments passed to the class
#' constructor, those values needs to be provided in `...`.
#'
#' @param env Environment. The instance environment.
#' @param cls Function. The class constructor.
#' @param method_name Character. The method name.
#' @param ... Arguments passed to the class constructor.
#' @param container_name Character. Container name of the instance environment.
#' @return The class method.
#' @export

class_method <- function(env, cls, method_name, ..., container_name = "method_env_") {

  # Init a class instance
  new_instance <- cls(..., env = new.env(parent = env))

  # Get the target method
  target_method <- new_instance[[method_name]]

  # Change the function env to target container
  bind_fn_2_env(env[[container_name]], target_method)

  # remove the instance
  rm(new_instance)

  return(target_method)
}


# sub_self_name -----------------------------------------------------------

sub_self_name <- function(fn, old_name, new_name) {

  # Check if names are characters
  if (!is.character(old_name)) stop("`old_name` is not a string!")
  if (!is.character(new_name)) stop("`new_name` is not a string!")

  # Get the function body
  fn_body <- body(fn)

  # Substitute old names with new names
  assign(old_name, as.symbol(new_name), envir = environment())
  body(fn) <- do.call(substitute, list(expr = fn_body, env = environment()))

  return(fn)
}

# is_instance -------------------------------------------------------------

#' Check whether an environment is an instance built by a class constructor
#'
#' This function returns `True` if the environment is an instance built by a
#' class constructor by checking the attribute "pseudo_oop_class"
#' of the class constructor.
#'
#' This function only works if the class constructor is registered.
#'
#' @param env Environment. The instance environment.
#' @param cls Function. The class constructor.
#' @return `TRUE` or `FALSE`.
#'
#' @examples
#'
#' tt <- BASE()
#' is_instance(tt, BASE)
#'
#' @seealso [register_class_ctor()]
#'
#' @export

is_instance <- function(env, cls) {
  attr(cls, "pseudo_oop_class")[1] == env$type
}


# is_subclass -------------------------------------------------------------

#' Check whether a class is a subclass of another class
#'
#' This function returns `TRUE` if the class is a subclass of another class by
#' checking the attribute "pseduo_oop_class" of the class constructors.
#'
#' This function only works if both class constructors are registered.
#'
#' @param child_cls Function. Child class constructor.
#' @param parent_cls Function. Parent class constructor.
#' @return `TRUE` or `FALSE`.
#'
#' @examples
#'
#' # Define a derived class constructor
#' myclass <- function(..., env = new.env(parent = parent.frame())) {
#'   env <- inherit(env, BASE, "myclass", ...)
#'
#'   myfunc_ <- function() 1 + 1
#'
#'   register_method(env, myfunc = myfunc_)
#'   return(env)
#' }
#'
#' register_class_ctor(myclass, "myclass", parent = BASE)
#'
#' is_subclass(myclass, BASE)
#'
#' @seealso [register_class_ctor()]
#'
#' @export

is_subclass <- function(child_cls, parent_cls) {
  attr(parent_cls, "pseudo_oop_class")[1] %in% attr(child_cls, "pseudo_oop_class")[-1]
}

# inherit -----------------------------------------------------------------

#' Declare inheritance inside a class constructor
#'
#' This function builds an instance of the parent class, with updated
#' class information.
#'
#' This function is assumed to be run inside a class constructor directly.
#' Otherwise, user needs to reset the `init_call` attribute by
#' calling `env$init_call <- sys.call()` inside the constructor directly.
#'
#' @param env Environment. The instance environment.
#' @param parent Function. The parent class constructor.
#' @param child_name Character. The derived class name.
#' @param ... Arguments passed to the parent class constructor.
#' @return A parent class instance with updated class information.
#'
#' @examples
#'
#' # Define a derived class constructor
#' myclass <- function(..., env = new.env(parent = parent.frame())) {
#'   env <- inherit(env, BASE, "myclass", ...)
#'
#'   myfunc_ <- function() 1 + 1
#'
#'   register_method(env, myfunc = myfunc_)
#'   return(env)
#' }
#'
#' register_class_ctor(myclass, "myclass", parent = BASE)
#'
#' @export

inherit <- function(env, parent, child_name, ...) {

  # Init a parent instance
  child <- parent(..., env = env)

  # Push the child class name
  child$class <- c(child_name, env$class)

  # Set the child instance type
  child$type <- env$class[1]

  # Reset the call
  # Warning: this only works if inherit is called within a class constructor directly
  child$init_call <- sys.call(which = 1)

  return(child)
}

# BASE --------------------------------------------------------------------

#' Base class constructor
#'
#' This class includes some basic methods and attributes.
#'
#' The base class provides the following attributes and methods:
#' \itemize{
#'   \item \code{class} : Class list.
#'   \item \code{type} : Class.
#'   \item \code{doc} : Doc string.
#'   \item \code{init_call} : The call that init this instance.
#'   \item \code{methods()} : Get all method names.
#'   \item \code{has_attr(attr_name)} : Whether the instance has the attribute.
#'   \item \code{set_attr(attr_name, attr_val)} : Set value for an attribute.
#'   \item \code{get_attr(attr_name)} : Get the value of an attribute.
#'   \item \code{del_attr(attr_name)} : Delete an attribute.
#'   \item \code{list_attr()} : Get all attributes name.
#'   \item \code{len()} : Length of the instance.
#'   \item \code{repr()} : Representation of an object.
#'   \item \code{string()} : String pepresentation of an object.
#' }
#'
#' @param ... Values that will be stored in the instance environment.
#' @param env Environment. The instance environment.
#' @return An environment with S3 class "pseudo_oop".
#'
#' @examples
#'
#' base_instance <- BASE(name = "a")
#' base_instance
#' base_instance$methods()
#'
#' @export
BASE <- function(..., env = new.env(parent = parent.frame())) {

  # Pass CMD check
  self <- NULL

  list2env(list(...), envir = env)

  env$class <- c("BASE")
  env$type <- env$class[1]
  env$doc <- ""
  env$init_call <- sys.call()

  methods_ <- function() names(self)[unlist(lapply(names(self), function(x) is.function(self[[x]])))]

  has_attr_ <- function(attr_name) attr_name %in% names(self)

  set_attr_ <- function(attr_name, attr_val) self[[attr_name]] <- attr_val

  get_attr_ <- function(attr_name) self[[attr_name]]

  del_attr <- function(attr_name) if (attr_name %in% names(self)) rm(attr_name, envir = self)

  list_attr_ <- function() names(self)

  len_ <- function() NULL

  repr_ <- function() deparse(self$init_call)

  string_ <- function() paste0("<", self$class[1], " object>")

  register_method(env,
                  methods = methods_,
                  has_attr = has_attr_,
                  set_attr = set_attr_,
                  get_attr = get_attr_,
                  list_attr = list_attr_,
                  repr = repr_,
                  string = string_,
                  len = len_)

  class(env) <- "pseudo_oop"
  return(env)
}

register_class_ctor(BASE, "BASE")



# print.pseudo_oop --------------------------------------------------------

#' S3 method of printing `pseudo_oop` object
#'
#' This function print the string representation of the object.
#'
#' @param x `pseudo_oop` object.
#' @param ... ignored.
#' @return No return value, called for side effects.
#' @export

print.pseudo_oop <- function(x, ...) {
  cli::cli_h3(x$string())
  return(invisible(NULL))
}




