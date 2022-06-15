test_that("calc_p_value() works", {
  expect_lt(calc_p_value(1, 1, c(1)), 1)
})

test_that("calc_p_value() passes tol = 0.01 for n_detect = 1, n_eval = 1, n_sel = 1 (if not, increase n_sim)", {
  set.seed(10086)
  expect_lt(abs(calc_p_value(1, 1, c(1)) - 0.05), 0.01)
})

test_that("calc_p_value() supports cache", {
  e <- new.env()
  p_value <- calc_p_value(1, 1, c(1), cache_env = e)
  p_value <- calc_p_value(1, 1, c(1), cache_env = e)
  expect_equal(p_value, p_value)
})

test_that("calc_p_value() works for n_detect = 0", {
  expect_equal(calc_p_value(0, 1, c(1)), 1)
})

