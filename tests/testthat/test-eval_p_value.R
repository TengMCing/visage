test_that("eval_p_value() works", {
  expect_equal(eval_p_value(0.054), FALSE)
})

test_that("eval_p_value() can use tol correctly", {
  expect_equal(eval_p_value(0.054, tol = 0.001), FALSE)
  expect_equal(eval_p_value(0.054, tol = 0.01), TRUE)
})
