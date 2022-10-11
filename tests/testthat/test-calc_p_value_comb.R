# test_that("calc_p_value_comb() passes tol = 0.01", {
#   set.seed(10086)
#   expect_true(all(abs(visage::calc_p_value_comb(c(TRUE, TRUE), 1, c(1, 1)) - 0.05) < 0.01))
#   expect_true(all(abs(visage::calc_p_value_comb(c(TRUE, TRUE), 2, c(1, 1)) - 0.0025) < 0.01))
# })
#
# test_that("calc_p_value_comb() supports cache", {
#   e <- new.env()
#   result <- visage::calc_p_value_comb(c(TRUE, TRUE), 1, c(1, 1), cache_env = e)
#   result1 <- visage::calc_p_value_comb(c(TRUE, TRUE), 1, c(1, 1), cache_env = e)
#   expect_equal(result, result1)
# })
#
# test_that("calc_p_value_comb() supports cache", {
#
#   expect_error(visage::calc_p_value_comb(c(TRUE, TRUE), 0, c(1, 1)),
#                "Number of evaluations equals to zero. Can not compute combinations.")
# })
