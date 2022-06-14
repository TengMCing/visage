test_that("calc_p_value_multi() works for comb = TRUE", {
  dat <- data.frame(lineup_id = c(1, 1, 2),
                    detected = c(TRUE, FALSE, TRUE),
                    n_sel = c(1, 1, 2))
  result <- calc_p_value_multi(dat, comb = TRUE, n_eval = 1:2)

  expect_true(all(result$p_value[[1]] - c(0.05, 1) < 0.01))
  expect_true(all(result$p_value[[2]] - c(0.1) < 0.01))
  expect_true(all(result$p_value[[3]] - c(0.0975) < 0.01))
})

test_that("calc_p_value_multi() works for comb = FALSE", {
  dat <- data.frame(lineup_id = c(1, 1, 2),
                    detected = c(TRUE, FALSE, TRUE),
                    n_sel = c(1, 1, 2))
  result <- calc_p_value_multi(dat, comb = FALSE, n_eval = 1:2)

  expect_true(all(result$p_value[1] - c(0.975) < 0.01))
  expect_true(all(result$p_value[2] - c(0.1) < 0.01))
})



test_that("calc_p_value_multi() supports cache", {
  dat <- data.frame(lineup_id = c(1, 1, 2),
                    detected = c(TRUE, FALSE, TRUE),
                    n_sel = c(1, 1, 2))
  e <- new.env()
  result <- calc_p_value_multi(dat, comb = TRUE, n_eval = 1:2, cache_env = e)
  result1 <- calc_p_value_multi(dat, comb = TRUE, n_eval = 1:2, cache_env = e)

  expect_equal(unlist(result$p_value), unlist(result1$p_value))
})
