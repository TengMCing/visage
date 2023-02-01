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

alpha_vi_survey <- vi_survey %>%
  filter(b == 0) %>%
  select(unique_lineup_id, x_dist, n) %>%
  group_by(unique_lineup_id) %>%
  summarise(across(x_dist:n, first)) %>%
  arrange(across(c(x_dist:n))) %>%
  mutate(setting_id = rep(1:12, each = 3))

alpha_vi_survey <- alpha_vi_survey %>%
  select(unique_lineup_id, setting_id) %>%
  right_join(filter(vi_survey, b == 0)) %>%
  select(unique_lineup_id, setting_id, selection, num_selection) %>%
  (function(x) {
    for (i in 1:20)
      x[[paste0("plot_", i)]] <-
        grepl(paste0("_", i, "_"), x$selection) |
        grepl(paste0("^", i, "_"), x$selection) |
        grepl(paste0("_", i, "$"), x$selection) |
        grepl(paste0("^", i, "$"), x$selection)
    x}
  ) %>%
  mutate(across(plot_1:plot_20, function(x) ifelse(num_selection, x/num_selection, 1/20))) %>%
  group_by(setting_id, unique_lineup_id) %>%
  summarise(across(plot_1:plot_20, sum)) %>%
  ungroup() %>%
  pivot_longer(plot_1:plot_20) %>%
  group_by(unique_lineup_id) %>%
  mutate(total = sum(value)) %>%
  ungroup() %>%
  (function(x) {
    for (id in x$unique_lineup_id) {
      current_ans <- vi_survey$answer[vi_survey$unique_lineup_id == id][1]
      x$value[x$unique_lineup_id == id & x$name == paste0("plot_", current_ans)] <- 0
    }
    x
  }) %>%
  group_by(setting_id, unique_lineup_id) %>%
  summarise(c_interesting = sum(value >= 19/total)) %>%
  group_by(setting_id) %>%
  summarise(c_interesting = mean(c_interesting)) %>%
  ungroup() %>%
  mutate(alpha = vinference::estimate_alpha_numeric(Zc = c_interesting, m0 = 19, K = 20)) %>%
  mutate(alpha_sum_sq_error = alpha$sum_sq_error, alpha = alpha$alpha) %>%
  right_join(alpha_vi_survey)


vi_survey <- vi_survey %>%
  left_join(alpha_vi_survey %>%
              group_by(setting_id) %>%
              summarise(across(everything(), first)) %>%
              select(x_dist, n, c_interesting, alpha, alpha_sum_sq_error))


p_value <- visage::calc_p_value_multi(vi_survey,
                                      lineup_id = unique_lineup_id,
                                      detect = detect,
                                      n_sel = num_selection,
                                      alpha = alpha)

vi_survey <- vi_survey %>%
  left_join(p_value, by = "unique_lineup_id") %>%
  mutate(attention_check = ifelse((type == "polynomial" & e_sigma < 0.5) | (type == "heteroskedasticity" & b > 64), TRUE, FALSE)) %>%
  mutate(null_lineup = ifelse(is.na(b) | b != 0, FALSE, TRUE)) %>%
  select(exp, unique_lineup_id:num, attention_check, null_lineup, response_time:conventional_p_value, p_value, reason:alpha_sum_sq_error)

vi_survey <- vi_survey %>%
  group_by(unique_lineup_id) %>%
  mutate(prop_detect = mean(weighted_detect)) %>%
  ungroup()

poly_lineup <- readRDS(here::here("data-raw/polynomials_lineup.rds"))
heter_lineup <- readRDS(here::here("data-raw/heter_lineup.rds"))
vi_lineup <- append(poly_lineup, heter_lineup)

usethis::use_data(vi_survey, overwrite = TRUE)
saveRDS(vi_lineup, here::here("data-raw/vi_lineup.rds"))
