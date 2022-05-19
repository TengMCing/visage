
# vi_experiment -----------------------------------------------------------

# class_VI_EXPERIMENT <- function(env = new.env(parent = parent.frame())) {
#
#   # Pass CMD check
#   self <- NULL
#
#   new_class(BASE, env = env, class_name = "VI_EXPERIMENT")
#
#   init_ <- function(folder = NULL) {
#     self$folder <- folder
#   }
#
#   decode_ <- function(filename = NULL, content = NULL) {
#     if (is.null(content)) content <- jsonlite::fromJSON(filename, simplifyVector = FALSE, ...)
#
#     responses <- self$extract_element(content = content, "response")
#     response_time <- self$extract_element(content = content, "rt")
#   }
#
#   json_2_list_ <- function(filename = NULL, content = NULL, ...) {
#     # Keep JSON array asis
#     if (is.null(content)) return(jsonlite::fromJSON(content, simplifyVector = FALSE, ...))
#
#     return(jsonlite::fromJSON(filename, simplifyVector = FALSE, ...))
#   }
#
#   extract_element_ <- function(filename = NULL,
#                                content = NULL,
#                                element_name = "response",
#                                keep_first = TRUE) {
#
#     if (is.null(content)) content <- self$json_2_list(filename)
#
#     if (keep_first) return(purrr::map(content, ~.x[[element_name]][[1]]))
#
#     return(purrr::map(content, ~.x[[element_name]]))
#   }
#
# }
