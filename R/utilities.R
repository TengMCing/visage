# define_pkg_fns ----------------------------------------------------------

#' Load functions from package namespaces into current environment
#'
#' This function loads functions from package namespaces via `::` and
#' assigns them to the preferred function names in the current environment.
#'
#' Preferred function names can be provide via named arguments
#' like `dplyr_filter = filter`.
#'
#' @param ... Functions. Preferred names can be provide via named arguments.
#' @param pkg Package.
#' @return No return value, called for side effects
#'
#' @examples
#' define_pkg_fns(select, filter, pkg = dplyr)
#' define_pkg_fns(select, dplyr_filter = filter, `%>%`, pkg = dplyr)
#'
#' @export
define_pkg_fns <- function(..., pkg = NULL) {

  # Capture function call
  fn_list <- as.list(sys.call())

  # pkg must be provided
  pkg <- fn_list$pkg
  if (is.null(pkg)) stop("`pkg` is missing!")

  # Extract ...
  fn_list$pkg <- NULL
  fn_list[[1]] <- NULL

  # Check if pref_names are provided via named arguments and delete all "`"
  pref_names <- names(fn_list)
  pref_names[pref_names == ""] <- as.character(fn_list)[pref_names == ""]
  pref_names <- gsub('`', '', pref_names)

  assign_list <- lapply(1:length(fn_list), function(i) {

    # Arguments in ... must be symbols
    if (!is.symbol(fn_list[[i]])) stop("`", as.expression(fn_list[[i]]), "` is not a symbol!")

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
#' must bind to names in the current environment.
#'
#' Pass character function names to `...` will cause error.
#'
#' @param ... Functions.
#' @param env Environment.
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
#' bind_fns_2_env(e$show_self, env = e)
#'
#' # Both point to the same environment
#' e$show_self()
#' e
#'
#' @export
bind_fns_2_env <- function(..., env = NULL) {

  # Capture function call
  fn_list <- as.list(sys.call())

  # env must be provided and has a self
  if (is.null(fn_list$env)) stop("`env` is missing!")
  env <- fn_list$env

  # Extract ...
  fn_list$env <- NULL
  fn_list[[1]] <- NULL

  for (fn in fn_list) {

    # Arguments in ... must be functions
    if (!is.function(eval(fn, envir = parent.frame()))) stop("`", as.expression(fn), "` is not a function!")

    # Bind the function environment to the target environment
    eval(substitute(environment(fn) <- env), envir = parent.frame())
  }

  return(invisible(NULL))
}

