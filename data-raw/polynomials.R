library(tidyverse)
library(visage)

# Set seed for the p-value simulation
set.seed(10086)

# SURVEY ------------------------------------------------------------------

class_SURVEY <- function(env = new.env(parent = parent.frame())) {

  bandicoot::new_class(env = env, class_name = "SURVEY")

  env$dat <- NULL
  env$cache_env <- new.env(parent = parent.env(env))

# ..init.. ----------------------------------------------------------------

  init_ <- function(survey_folder = NULL, k = 20, lineup_dat = NULL, lineup_ord = NULL) {
    self$survey_folder <- survey_folder
    self$k <- k
    self$lineup_dat <- lineup_dat
    self$lineup_ord <- lineup_ord
  }


# get_responses -----------------------------------------------------------

  get_responses_ <- function(uuid, survey_folder = self$survey_folder, k = self$k) {
    # Map user choices to user info
    user_info_dict <- list(age = c("18-24", "25-39", "40-54", "55-64", "65 or above"),
                           edcuation = c("High School or below", "Diploma and Bachelor Degree", "Honours Degree", "Masters Degree", "Doctoral Degree"),
                           pronoun = c("He", "She", "They", "Other"),
                           experience = c("Yes", "No"))

    survey_result <- jsonlite::fromJSON(here::here(glue::glue("data-raw/{survey_folder}/{uuid}.txt")),
                                        simplifyVector = FALSE)

    response_time <- map_dbl(survey_result, ~.x$rt)

    # The third page is the user information page
    user_information <- str_split(survey_result[[3]]$response$user_information, ",")[[1]]

    # The first response is the Prolific ID, we replace the 2 to 5 elements by the corresponding choices
    user_information[2:5] <- imap_chr(user_information[-1], ~user_info_dict[[.y]][as.integer(.x)])

    names(user_information) <- c("prolific_id", "age_group", "education", "pronoun", "previous_experience")


    # Get the lineup responses (list of 20)
    # The first four pages are not lineups
    lineup_respone <- str_split(map_chr(survey_result[-(1:4)], ~.x$response$response), ",")

    # handle selections
    # The first response is the selection
    selections <- map_chr(lineup_respone, ~.x[1])

    # handle reasons
    # The second response is the reason
    reasons_dict <- c("Outlier(s)", "Cluster(s)", "Shape")

    # If the reason is "1", "2" or "3", then map it using the dictionary,
    # otherwise, keep the user provided reason or "NA"
    reasons <- map_chr(lineup_respone, ~if(.x[2] %in% c("1", "2", "3")) {
      reasons_dict[as.integer(.x[2])]
    } else {
      .x[2]
    })

    # handle confidence
    # The third response is the confidence
    confidence_dict <- c("Not at all", "Slightly", "Moderately", "Very", "Extremely")

    # If the confidence is "1", "2", "3", "4" or "5", then map using the dictionary,
    # otherwise, keep the user provided confidence ("NA")
    confidence <- map_chr(lineup_respone, ~if(.x[3] %in% c("1", "2", "3", "4", "5")) {
      confidence_dict[as.integer(.x[3])]
    } else {
      .x[3]
    })


    # Output a tibble
    # 24 rows, 20 lineup responses
    tibble(page = 1:length(response_time),
           response_time = response_time,
           set = uuid,
           num = c(rep(NA, 4), 1:k),
           selection = c(rep(NA, 4), selections),
           num_selection = c(rep(NA, 4), str_count(selections, "_") + 1),
           reason = c(rep(NA, 4), reasons),
           confidence = c(rep(NA, 4), confidence),
           prolific_id = unname(user_information['prolific_id']),
           age_group = unname(user_information['age_group']),
           education = unname(user_information['education']),
           pronoun = unname(user_information['pronoun']),
           previous_experience = unname(user_information['previous_experience'])) %>%
      mutate(reason = ifelse(selection == "NA", NA, reason)) %>%
      mutate(confidence = ifelse(selection == "NA", NA, confidence)) %>%
      mutate(selection = ifelse(selection == "NA", "0", selection)) %>%
      mutate(num_selection = ifelse(selection == "0", 0, num_selection))
  }


# detect_or_not -----------------------------------------------------------

  detect_or_not_ <- function(selection, answer) {
    detect <- imap_lgl(selection, ~answer[.y] %in% as.integer(str_split(.x, "_")[[1]]))
    detect[is.na(selection)] <- NA
    detect
  }


# process_responses -------------------------------------------------------

  process_responses_ <- function(survey_folder = self$survey_folder,
                                 k = 20,
                                 lineup_dat = self$lineup_dat,
                                 lineup_ord = self$lineup_ord) {

    # Produce a tibble of id, lineup_dat_id, set, num
    tbl_dat <- tibble(id = 1:length(unlist(lineup_ord)),
                      lineup_id = unlist(lineup_ord)) %>%
      mutate(set = (id-1) %/% k + 1) %>%
      mutate(num = (id-1) %% k + 1)

    response_dat <- NULL

    # Files in the folder
    file_names <- list.files(here::here(glue::glue("data-raw/{survey_folder}")))

    # Get all filenames aligned with the format "\digits.txt", and extract "\digits"
    uuids <- sort(as.integer(gsub("^(\\d+).txt$", "\\1", file_names[grep("^\\d+.txt", file_names)])))

    for (uuid in uuids) {
      if (is.null(response_dat)) {
        response_dat <- self$get_responses(uuid, survey_folder, k)
      } else {
        response_dat <- bind_rows(response_dat, self$get_responses(uuid, survey_folder, k))
      }
    }


    self$dat <- response_dat %>%

      # remove prolific ids
      select(-prolific_id) %>%

      # Left join to add lineup_id
      left_join(select(tbl_dat, -id), by = c("set", "num")) %>%

      # Add metadata list to column metadata
      mutate(metadata = map(lineup_id, ~lineup_dat[[.x]]$metadata)) %>%

      # Unnest the metadata column and clean up some metadata
      unnest_wider(metadata)  %>%
      rename(answer = ans) %>%

      # Check if the answer being detected
      mutate(detect = self$detect_or_not(selection, answer)) %>%

      # Perform the conventional tests and get the p-values
      mutate(conventional_p_value = map_dbl(lineup_id,
                                            function(lineup_id) {
                                              if (is.na(lineup_id)) return(NA)

                                              POLY_MODEL$test(filter(lineup_dat[[lineup_id]]$data, null == FALSE))$p_value
                                            })) %>%

      # Discard non-lineup pages
      filter(page > 4)

    p_value <- self$dat %>%
      calc_p_value_multi(detected = "detect", n_sel = "num_selection", cache_env = self$cache_env, n_sim = 100000)

    self$dat <- self$dat %>%
      left_join(p_value)

    return(self$dat)
  }

  bandicoot::register_method(env,
                             ..init.. = init_,
                             get_responses = get_responses_,
                             detect_or_not = detect_or_not_,
                             process_responses = process_responses_)

  return(env)
}


SURVEY <- class_SURVEY()
poly_survey <- SURVEY$instantiate(survey_folder = "survey",
                                  k = 20,
                                  lineup_dat = readRDS(here::here("data-raw/third_experiment_dat.rds")),
                                  lineup_ord = read_csv(here::here("data-raw/third_experiment_order.txt"), col_names = FALSE) %>%
                                    t() %>%
                                    as.data.frame() %>%
                                    as.list() %>%
                                    unname())

poly_survey$process_responses()

polynomials <- poly_survey$dat
polynomials_lineup <- poly_survey$lineup_dat
names(polynomials_lineup) <- unclass(glue::glue("lineup_{1:length(polynomials_lineup)}"))
polynomials_lineup <- map(polynomials_lineup, ~{.x$metadata$answer <- .x$metadata$ans; .x$metadata$ans <- NULL; .x})

usethis::use_data(polynomials, overwrite = TRUE)
saveRDS(polynomials_lineup, here::here("data-raw/polynomials_lineup.rds"))
