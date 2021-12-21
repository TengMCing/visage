#' Base class constructor
#'
#' This class includes some basic methods and attributes.
#'
#' details
#'
#' @param env Environment. The instance environment.
#' @param ... Values that will be stored in the instance environment.
#' @return An environment with S3 class "pseudo_oop".
#'
#' @examples
#'
#' base_instance <- BASE(name = "a")
#' base_instance
#' base_instance$methods()
#'
#' @export
BASE <- function(env = new.env(parent = parent.frame()), ...) {

  # Base class constructor

  # This template provides some basic methods and attributes

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

attr(BASE, "parent_class") <- c()

is_instance <- function(env, cls) {
  cls == env$type
}

is_subclass <- function(child_cls, parent_cls) {
  parent_cls %in% attr(child_cls, "parent_class")
}

print.pseudo_oop <- function(x, ...) {
  cli::cli_h3(x$string())
  return(invisible(NULL))
}

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
#' by specifying a string in `container_name`.
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
