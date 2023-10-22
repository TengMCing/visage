test_that("exact_dist() works and adds up to 1", {
  expect_equal(sum(visage::exact_dist(10)), 1)
})

test_that("exact_dist(1) works and it is c(0.95, 0.05)", {
  expect_equal(unname(visage::exact_dist(1)[1]), 0.95)
})
