poly <- visage::polynomials %>%
  mutate(exp = 3)
heter <- visage::heter %>%
  mutate(exp = 4)
fifth_experiment <- visage::fifth_experiment %>%
  mutate(exp = 5) %>%
  mutate(lineup_id = as.numeric(lineup_id))

library(tidyverse)
vi_survey <- fifth_experiment %>%
  bind_rows(poly) %>%
  bind_rows(heter)

poly_lineup <- get_polynomials_lineup()
heter_lineup <- get_heter_lineup()
vi_lineup <- append(poly_lineup, heter_lineup)

usethis::use_data(vi_survey, overwrite = TRUE)
saveRDS(vi_lineup, here::here("data-raw/vi_lineup.rds"))
