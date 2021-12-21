# RAND_VAR <- function(var_env = new.env(parent = parent.frame()), name, ...) {
#
#   # The RAND_VAR class constructor
#
#   var_env$name <- name
#   var_env$self <- var_env
#   var_env$parameters <- list()
#
#   gen_ <- function(...) {
#
#   }
#
#   update_ <- function(...) self$parameters <- modifyList(self$parameters, list(...))
#
#   methods_ <- function() names(self)[unlist(lapply(names(self), function(x) is.function(self[[x]])))]
#   attrs_ <- function() {}
#
#   register_method(var_env, gen = gen_, update = update_, methods = methods_)
#   class(var_env) <- "RAND_VAR"
#   return(var_env)
# }
#
