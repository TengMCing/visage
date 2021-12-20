# define_pkg_fns ----------------------------------------------------------

#' Load functions from package namespaces into current environment
#'
#' This function loads functions from package namespaces via `::` and
#' assigns them to the preferred function names in the current environment.
#'
#' Preferred function names can be provide via named arguments
#' like `dplyr_filter = filter`.
#'
#' @param pkg Package.
#' @param ... Functions. Preferred names can be provide via named arguments.
#' @return No return value, called for side effects
#'
#' @examples
#' define_pkg_fns(pkg = dplyr, select, filter)
#' define_pkg_fns(dplyr, select, dplyr_filter = filter, `%>%`)
#'
#' @export
define_pkg_fns <- function(pkg, ...) {

  # Capture function call
  fn_list <- as.list(sys.call())

  # Extract `...`, check how `pkg` is provided
  if (is.null(fn_list$pkg)) {
    pkg <- fn_list[[2]]
    fn_list[1:2] <- NULL
  } else {
    pkg <- fn_list$pkg
    fn_list$pkg <- NULL
    fn_list[[1]] <- NULL
  }

  # Check if `pref_names` are provided via named arguments and delete all "`"
  # A unnamed list will return NULL, a named list but without names will return empty strings
  pref_names <- names(fn_list)

  if (is.null(pref_names)) {
    pref_names <- as.character(fn_list)
  } else {
    pref_names[pref_names == ""] <- as.character(fn_list)[pref_names == ""]
  }
  pref_names <- gsub('`', '', pref_names)

  assign_list <- lapply(1:length(fn_list), function(i) {

    # Arguments in `...` must be symbols
    if (!is.symbol(fn_list[[i]])) stop("`", as.expression(fn_list[[i]]), "` is not a symbol!")

    # `Base` an `sum` are not used, they are here to pass the CMD check
    eval(substitute(`::`(base, sum),
                    list(base = pkg, sum = fn_list[[i]])))
  })

  names(assign_list) <- pref_names
  list2env(assign_list, envir = parent.frame())

  return(invisible(NULL))
}





# bind_fns_2_env ----------------------------------------------------------

#' Bind functions of the current environment to a target environment
#'
#' This function is equivalent to `environment(fn) <- env`. Hence functions
#' must bind to names.
#'
#' Pass character function names to `...` will cause error.
#'
#' @param env Environment.
#' @param ... Functions.
#' @return No return value, called for side effects
#'
#' @examples
#' # Access the associated environment inside a function
#'
#' self <- NULL
#' e <- new.env()
#'
#' # The associated environment needs to have a reference to itself
#' e$self <- e
#'
#' e$show_self <- function() return(self)
#'
#' # The function can only access the global variable `self`
#' e$show_self()
#'
#' # Bind the function to the environment `e`
#' bind_fns_2_env(env = e, e$show_self)
#'
#' # Both point to the same environment
#' e$show_self()
#' e
#'
#' @export
bind_fns_2_env <- function(env, ...) {

  # Capture function call
  fn_list <- as.list(sys.call())

  # `env` must be provided
  if (!is.environment(env)) stop("`env` is not an environment!")

  # Extract `...`, check how `env` is provided
  if (is.null(fn_list$env)) {
    fn_list[1:2] <- NULL
  } else {
    fn_list[[1]] <- NULL
    fn_list$env <- NULL
  }

  for (fn in fn_list) {

    # Arguments in `...` must be functions
    if (!is.function(eval(fn, envir = parent.frame()))) stop("`", as.expression(fn), "` is not a function!")

    # Change the function environment to the target environment
    eval(substitute(environment(fn) <- env), envir = parent.frame())
  }

  return(invisible(NULL))
}


# register_method ---------------------------------------------------------

#' Register method for a class or an instance
#'
#' This function register a method which needs to be a function
#' @noRd

register_method <- function(env, ...) {

  # Capture function call
  fn_list <- as.list(sys.call())

  # `env` must be provided
  if (!is.environment(env)) stop("`env` is not an environment!")

  # Extract `...`, check how `env` is provided
  if (is.null(fn_list$env)) {
    fn_list[1:2] <- NULL
  } else {
    fn_list[[1]] <- NULL
    fn_list$env <- NULL
  }

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
    bind_fns_2_env(env, env[[fn_names[i]]])
  }

  return(invisible(NULL))
}
