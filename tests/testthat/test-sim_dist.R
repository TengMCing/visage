test_that("sim_dist() works and adds up to 1", {
  expect_equal(sum(visage::sim_dist(c(2, 2, 3), n_sim = 1000)), 1)
})


test_that("sim_dist() passes tol = 0.01 for n_eval = 1, n_sel = 1 (if not, increase n_sim)", {
  expect_lt(abs(visage::sim_dist(c(1))[1] - 0.95), 0.01)
})
