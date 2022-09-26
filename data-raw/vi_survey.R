library(tidyverse)

poly <- visage::polynomials %>%
  mutate(exp = 3)
heter <- visage::heter %>%
  mutate(exp = 4)
fifth_experiment <- visage::fifth_experiment %>%
  mutate(exp = 5) %>%
  mutate(lineup_id = as.numeric(lineup_id))


vi_survey <- fifth_experiment %>%
  bind_rows(poly) %>%
  bind_rows(heter)

set.seed(10086)

p_value <- visage::calc_p_value_multi(vi_survey,
                                      lineup_id = "unique_lineup_id",
                                      detected = "detect",
                                      n_sel = "num_selection",
                                      cache_env = new.env())

vi_survey <- vi_survey %>%
  left_join(p_value, by = "unique_lineup_id") %>%
  select(exp, unique_lineup_id:conventional_p_value, p_value, reason:n)

poly_lineup <- readRDS(here::here("data-raw/polynomials_lineup.rds"))
heter_lineup <- readRDS(here::here("data-raw/heter_lineup.rds"))
vi_lineup <- append(poly_lineup, heter_lineup)

usethis::use_data(vi_survey, overwrite = TRUE)
saveRDS(vi_lineup, here::here("data-raw/vi_lineup.rds"))
