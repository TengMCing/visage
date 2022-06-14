test_that("define_pkg_fn() can import function from library", {
  define_pkg_fn(dplyr, select)
  expect_equal(select(cars, speed), dplyr::select(cars, speed))
})


test_that("define_pkg_fn() can import function with custom name from library", {
  define_pkg_fn(dplyr, dplyr_s = select)
  expect_equal(dplyr_s(cars, speed), dplyr::select(cars, speed))
})

test_that("define_pkg_fn() accept pkg via named argument", {
  define_pkg_fn(dplyr_s = select, pkg = dplyr)
  expect_equal(dplyr_s(cars, speed), dplyr::select(cars, speed))
})

test_that("define_pkg_fn() can handle special character (e.g. %>%)", {
  define_pkg_fn(dplyr_s = select, `%>%`, pkg = dplyr)
  expect_equal(dplyr_s(cars, speed), dplyr::select(cars, speed))
})


