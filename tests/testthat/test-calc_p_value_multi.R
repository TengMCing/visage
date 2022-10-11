test_that("calc_p_value_multi() works for comb = FALSE", {
  set.seed(10086)
  dat <- data.frame(lineup_id = c(1, 1, 2),
                    detected = c(TRUE, FALSE, TRUE),
                    n_sel = c(1, 1, 2))
  result <- visage::calc_p_value_multi(dat, lineup_id, detected, n_sel)

  expect_true(all(result$p_value[1] - c(0.952) < 0.01))
  expect_true(all(result$p_value[2] - c(0.525) < 0.01))
})

