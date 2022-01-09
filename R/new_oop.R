
# register_method_v2 ------------------------------------------------------


register_method_v2 <- function(obj, ..., container_name = "..method_env..", self_name = "self") {

  # Capture function call
  fn_list <- as.list(sys.call())

  # `obj` must be an environment
  if (!is.environment(obj)) stop("`obj` is not an environment!")

  # `container_name` and `self_name` must be provided as characters
  if (!is.character(container_name)) stop("`container_name` must be characters!")
  if (!is.character(self_name)) stop("`self_name` must be characters!")

  # Check if container exists
  if (container_name %in% names(obj)) {

    # Check if the container is an environment
    if (!is.environment(obj[[container_name]])) stop(container_name, " exists, but it is not an environment! Consider remove it.")

    # Check if the container is empty (except self)
    if (sum(names(obj[[container_name]]) != self_name) > 0) warning("The container is not empty!")

    # Check if the container is the child of the parent of the object
    if (!identical(parent.env(obj[[container_name]]), parent.env(obj))) stop(container_name, " exists, but it is not a child of the parent of the object! Consider remove it.")

    # Check if self exists
    if (self_name %in% names(obj[[container_name]])) {

      # Check if self points to the object
      if (!identical(obj[[container_name]][[self_name]], obj)) stop(self_name, " exists, but it does not point to the object! Consider remove it.")

    } else {

      # Otherwise, create self for the object
      obj[[container_name]][[self_name]] <- obj
    }

  } else {

    # Otherwise, create the container and self
    # the parent of the container is the parent of the object
    obj[[container_name]] <- new.env(parent = parent.env(obj))
    obj[[container_name]][[self_name]] <- obj

    # Give the container a container class
    class(obj[[container_name]]) <- "visage_oop_container"
  }

  # Check if caller exists
  if ("..caller.." %in% names(obj)) {

    # Check if the caller is an environment
    if (!is.environment(obj$..caller..)) stop("`..caller..` exists, but it is not an environment! Consider remove it.")

    # Check if the caller has a list called "..method_ref.."
    if (!"..method_ref.." %in% names(obj$..caller..)) stop("`..method_ref..` missing!")
    if (!is.list(obj$..caller..$..method_ref..)) stop("`..method_ref..` is not a list!")

  } else {

    # New a caller environment
    # The parent of the caller should be the object
    obj$..caller.. <- new.env(parent = obj)

    # Method references to the original functions (for memory saving)
    obj$..caller..$..method_ref.. <- list()

    # Special method to actually call the method
    obj$..caller..$..call.. <- function(fn_name, ori_call, exec_env,
                                        container_name = "..method_env..",
                                        self_name = "self") {

      # Get self using the self name
      self <- eval(as.symbol(self_name))

      # Get a copy of the method
      fn <- self$..caller..$..method_ref..[[fn_name]]

      # Let it run in the container
      environment(fn) <- self[[container_name]]

      # Replace the original call with this function
      ori_call[[1]] <- fn

      # Call from the target environment
      eval(ori_call, envir = exec_env)
    }

    # Let the ..call.. runs inside the container
    environment(obj$..caller..$..call..) <- obj[[container_name]]

    # Set the caller to caller class
    class(obj$..caller..) <- "visage_oop_caller"
  }

  # Extract `...`, check how `obj` is provided
  if (is.null(fn_list$obj)) {
    fn_list[1:2] <- NULL
  } else {
    fn_list[[1]] <- NULL
    fn_list$obj <- NULL
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

    # Make a copy in the "..method_ref.."
    obj$..caller..$..method_ref..[[fn_names[i]]] <- eval_fn

    # Set a wrapper method
    obj[[fn_names[i]]] <- function(..., self_name = "self") {

      # Get the function name from the attribute
      fn_name <- attr(sys.function(), "method_name")

      # Get the self using self_name
      self <- eval(as.symbol(self_name))

      # Use the ..call.. method to call the function
      self$..caller..$..call..(fn_name = fn_name,
                               ori_call = sys.call(),
                               exec_env = parent.frame())
    }

    # Get the formals of the original function
    ori_formals <- formals(eval_fn)

    # Set the self_name argument to be "self", or append it at the end
    ori_formals$self_name <- "self"

    # Replace ... with actual formals (+ self_name)
    formals(obj[[fn_names[i]]]) <- ori_formals

    # RESET the formals will potentially drop attributes and reset environment!

    # Set the function name
    attr(obj[[fn_names[i]]], "method_name") <- fn_names[i]

    # Let the function binds to the ..method_env..
    environment(obj[[fn_names[i]]]) <- obj[[container_name]]

    # Give function a class to print nicer output
    class(obj[[fn_names[i]]]) <- "visage_oop_method"
  }

  return(invisible(NULL))
}

print.visage_oop_container <- function(x, ...) {

  self_name <- list(...)$self_name
  if (is.null(self_name)) self_name <- "self"

  results <- paste0("<Method container: ",
                    gsub('<environment: (.*)>', '\\1', utils::capture.output(print.default(x))[1]),
                    "> of [",
                    ifelse(is.null(x[[self_name]]$..instantiated..) || x[[self_name]]$..instantiated..,
                           "Object", "Class"),
                    ": ",
                    gsub('<environment: (.*)>', '\\1', utils::capture.output(print.default(x[[self_name]]))[1]),
                    "]\n  - Type: ",
                    ifelse(is.null(x[[self_name]]$..type..), "unknown", x[[self_name]]$..type..),
                    "\n  - Contents:"
                    )

  for (name in sort(names(x))) results <- paste0(results, "\n    - ", name)

  cli::cli_h3(results)


}

print.visage_oop_caller <- function(x, ...) {

  self_name <- list(...)$self_name
  if (is.null(self_name)) self_name <- "self"

  results <- paste0("<Caller: ",
                    gsub('<environment: (.*)>', '\\1', utils::capture.output(print.default(x))[1]),
                    "> of [",
                    ifelse(is.null(parent.env(x)$..instantiated..) || parent.env(x)$..instantiated..,
                           "Object", "Class"),
                    ": ",
                    gsub('<environment: (.*)>', '\\1', utils::capture.output(print.default(parent.env(x)))[1]),
                    "] \n  - Type: ",
                    ifelse(is.null(parent.env(x)$..type..), "unknown", parent.env(x)$..type..),
                    "\n  - Methods:"
  )

  for (name in sort(names(x$..method_ref..))) results <- paste0(results, "\n    - ", name)

  cli::cli_h3(results)
}


print.visage_oop_method <- function(x, ...) {

  self_name <- list(...)$self_name
  if (is.null(self_name)) self_name <- "self"

  fn <- environment(x)[[self_name]]$..caller..$..method_ref..[[attr(x, "method_name")]]
  ori_env <- environment(fn)
  environment(fn) <- .GlobalEnv

  # Drop all attributes and class information
  body(fn) <- body(fn)
  class(fn) <- NULL

  results <- paste0("<Method> \n  - Method name: ",
                    attr(x, "method_name"),
                    "\n  - Type: ",
                    ifelse(is.null(environment(x)[[self_name]]$..type..), "unknown", environment(x)[[self_name]]$..type..),
                    "\n  - Original environment: ",
                    gsub('<environment: (.*)>', '\\1', utils::capture.output(print.default(ori_env))[1]),
                    "\n  - Body: \n",
                    paste0(capture.output(print(fn)), collapse = "\n    "),
                    "\nExecute in <Method container: ",
                    gsub('<environment: (.*)>', '\\1', utils::capture.output(print.default(environment(x)))[1]),
                    "> of [Object: ",
                    gsub('<environment: (.*)>', '\\1', utils::capture.output(print.default(environment(x)[[self_name]]))[1]),
                    "]"
  )

  # Replace all {} with {{}}
  results <- gsub("}", "}}", gsub("{", "{{", results, fixed = TRUE), fixed = TRUE)
  cli::cli_h3(results)
}


copy_attr_v2 <- function(obj, ..., avoid = c("..method_env..", "..init_call..", "..caller..")) {

  if (!is.environment(obj)) stop("`obj` is not an environment!")

  # Get list of classes
  class_list <- list(...)

  if (length(class_list) == 0) stop("At least provide one source to copy from!")

  for (cls in class_list) {

    method_list <- list()
    attr_list <- list()

    cls_methods <- names(cls)[unlist(lapply(names(cls), function(x) is.function(cls[[x]])))]
    cls_dict <- names(cls)

    # Get list of methods
    for (method_name in cls_methods) {
      if (method_name %in% avoid) next
      method_list[[method_name]] <- cls$..caller..$..method_ref..[[method_name]]
    }

    # Get list of attributes
    for (attr_name in setdiff(cls_dict, cls_methods)) {
      if (attr_name %in% avoid) next
      attr_list[[attr_name]] <- cls[[attr_name]]
    }

    method_list$obj <- obj

    # Bind methods to the target container
    do.call(register_method_v2, method_list)

    # Set attributes
    list2env(attr_list, envir = obj)
  }
}

new_class_v2 <- function(..., obj = new.env(parent = parent.frame()), class_name = NULL) {

  # Class should has a name
  if (is.null(class_name)) stop("`class_name` is null!")

  obj$..class.. <- c()

  # Methods will be overrided by the left classes
  for (parent in rev(list(...))) {

    if (parent$..instantiated..) stop("Parent is not a class!")

    obj$..class.. <- c(obj$..class.., parent$..class..)

    # Copy all the methods and attributes from the class/instance
    # except the container, the init_call, and the class information
    copy_attr_v2(obj, parent, avoid = c("..method_env..",
                                        "..caller..",
                                        "..init_call..",
                                        "..class..",
                                        "..type..",
                                        "..instantiated.."))
  }

  obj$..class.. <- c(class_name, obj$..class..)
  obj$..type.. <- class_name
  obj$..instantiated.. <- FALSE

  # Set S3 class
  class(obj) <- "visage_oop"

  # Return the class
  return(obj)
}



class_BASE2 <- function(obj = new.env(parent = parent.frame())) {

  # Pass CMD check
  self <- NULL

  # Define a new class
  new_class_v2(obj = obj, class_name = "BASE2")

  # Default instantiation method
  instantiation_ <- function(..., obj = new.env(parent = parent.frame())) {

    # Create an new object, called the class `..new..` method
    # init_call -> caller call -> two evals call -> one final call
    # -4 -> -3 -> -1 -> 0
    init_call <- sys.call(-4)
    self$..new..(obj = obj, init_call = init_call)

    # Init, called the object `..init..` method
    obj$..init..(...)

    # `instantiation` method should return the environment by convention
    return(obj)
  }

  new_ <- function(obj = new.env(parent = parent.frame()), init_call = sys.call(-4)) {

    # Copy all the methods and attributes from the class/instance
    # except the container, the instantiation method, init_call
    visage:::copy_attr_v2(obj, self, avoid = c("..method_env..",
                                              "..caller..",
                                           "instantiation",
                                           "..init_call..",
                                           "..instantiated.."))

    # Set the `init_call`
    obj$..init_call.. <- init_call

    # Mark the object as an instance
    obj$..instantiated.. <- TRUE

    # Set the S3 class
    class(obj) <- "visage_oop"

    # `..new..` method should return the object by convention
    return(obj)
  }

  # Default init method
  init_ <- function(...) return(invisible(NULL))

  methods_ <- function() names(self)[unlist(lapply(names(self), function(x) is.function(self[[x]])))]

  has_attr_ <- function(attr_name) attr_name %in% names(self)

  set_attr_ <- function(attr_name, attr_val) self[[attr_name]] <- attr_val

  get_attr_ <- function(attr_name) self[[attr_name]]

  del_attr <- function(attr_name) if (attr_name %in% names(self)) rm(attr_name, envir = self)

  dict_ <- function() names(self)

  len_ <- function() NULL

  repr_ <- function() paste(deparse(self$..init_call.., width.cutoff = 500L), collapse = " ")

  str_ <- function() {
    if (self$..instantiated..) {
      return(paste0("<", self$..type.., " object>"))
    } else {
      return(paste0("<", self$..type.., " class>"))
    }
  }

  register_method_v2(obj,
                  instantiation = instantiation_,
                  ..new.. = new_,
                  ..init.. = init_,
                  ..methods.. = methods_,
                  has_attr = has_attr_,
                  set_attr = set_attr_,
                  get_attr = get_attr_,
                  ..dict.. = dict_,
                  ..repr.. = repr_,
                  ..str.. = str_,
                  ..len.. = len_)
  return(obj)
}
