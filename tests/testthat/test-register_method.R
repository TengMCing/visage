test_that("register_method works", {
  a <- function() self$x
  e <- new.env()
  e$x <- 0
  register_method(e, a = a)

  expect_equal(e$a(), 0)
})

test_that("register_method works", {
  a <- function() self$x
  b <- function() self$x + 1
  e <- new.env()
  e$x <- 0
  register_method(e, a = a, b = b)

  expect_equal(e$b(), 1)
})

test_that("register_method container_name works", {
  a <- function() self$x
  e <- new.env()
  e$x <- 0
  register_method(e, a = a, container_name = "aa")

  expect_equal(e$a(), 0)
})

test_that("register_method container (env) error works", {
  a <- function() self$x
  e <- new.env()
  e$method_env_ <- 1
  e$x <- 0

  expect_error(register_method(e, a = a),
               "method_env_ exists, but it is not an environment! Consider remove it.",
               ignore.case = TRUE)
})

test_that("register_method container (diff env) error works", {
  a <- function() self$x
  e <- new.env()
  d <- new.env()
  e$method_env_ <- new.env(parent = d)
  e$x <- 0

  expect_error(register_method(e, a = a),
               " exists, but it is not a child of the parent of the instance environment! Consider remove it.",
               ignore.case = TRUE)
})

test_that("register_method container warning works", {
  a <- function() self$x
  e <- new.env()
  e$method_env_ <- new.env(parent = parent.env(e))
  e$method_env_$b <- 1
  e$x <- 0

  expect_warning(register_method(e, a = a),
                 "The container is not empty!",
                 ignore.case = TRUE)
})

test_that("register_method self_name works", {
  a <- function() this$x
  e <- new.env()
  e$x <- 0
  register_method(e, a = a, self_name = "this")

  expect_equal(e$a(), 0)
})

test_that("register_method self_name error works", {
  a <- function() this$x
  e <- new.env()
  e$method_env_ <- new.env(parent = parent.env(e))
  e$method_env_$self <- new.env()
  e$x <- 0

  expect_error(register_method(e, a = a, self_name = "self"),
               "self exists, but it is not the same as the provided environment! Consider remove it.",
               ignore.case = TRUE)
})


