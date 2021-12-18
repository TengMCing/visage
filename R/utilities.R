# define_pkg_funcs --------------------------------------------------------

#' Load functions from package namespaces into current environment
#'
#' This function loads functions from package namespaces via `::` and
#' assigns them to the preferred function names in the current environment.
#'
#' If the length of `pref_names` and `func_names` does not match, `func_names`
#' will repleace `pref_names`.
#'
#' @param func_names Character. A vector of function names.
#' @param pkg_names Character. A vector of package names.
#' @param pref_names Character. A vector of preferred function names.
#' @return No return value, called for side effects
#'
#' @examples
#' define_pkg_funcs("filter", "dplyr")
#' define_pkg_funcs(c("filter", "%>%"), rep("dplyr", 2))
#'
#' @export
define_pkg_funcs <- function(func_names, pkg_names, pref_names = func_names) {

  if (length(pkg_names) == 1) pkg_names <- rep(pkg_names, length = length(func_names))

  # Collect functions from packages
  fns <- lapply(1:length(func_names),
                function(i) {
                  expr <- substitute(`::`(base, sum),
                                     list(base = pkg_names[i], sum = func_names[i]))
                  eval(expr)
                }
  )

  # Assign them back to the parent frame
  if (length(unique(pref_names)) == length(unique(func_names))) {
    names(fns) <- pref_names
  } else {
    warning("Length of `pref_names` doesn't match the length of `func_names`!")
    names(fns) <- func_names
  }

  list2env(fns, envir = parent.frame())

  return(invisible(NULL))
}
