
# vi_experiment -----------------------------------------------------------

class_VI_EXPERIMENT <- function(env = new.env(parent = parent.frame())) {

  # Pass CMD check
  self <- NULL

  new_class(BASE, env = env, class_name = "VI_EXPERIMENT")

  json_2_list_ <- function(filename = NULL, content = NULL, ...) {
    # Keep JSON array asis
    if (is.null(content)) return(jsonlite::fromJSON(content, simplifyVector = FALSE, ...))

    return(jsonlite::fromJSON(filename, simplifyVector = FALSE, ...))
  }

  extract_response_ <- function(filename = NULL, content = NULL) {

    if (is.null(content)) content <- self$json_2_list(filename)

  }
}


purrr::map(list(list(list(rt = 1), list(rt = 2))), ~purrr::map(.x, ~.x$rt)) |> unlist()
