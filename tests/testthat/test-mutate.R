test_that("mutate adds derived columns", {
  out <- mutate(mtcars, ratio = mpg / cyl)

  expect_s3_class(out, "data.table")
  expect_true("ratio" %in% names(out))
  expect_equal(out$ratio[[1]], mtcars$mpg[[1]] / mtcars$cyl[[1]])
})

test_that("transform verbs resolve values from their caller", {
  data <- data.frame(value = 1:3)

  run_transform <- function(x) {
    multiplier <- 3
    transform(x, result = value * multiplier)
  }
  run_mutate <- function(x) {
    offset <- 2
    mutate(x, result = value + offset)
  }
  run_transmute <- function(x) {
    denominator <- 2
    transmute(x, result = value / denominator)
  }

  expect_equal(run_transform(data)$result, c(3, 6, 9))
  expect_equal(run_mutate(data)$result, c(3, 4, 5))
  expect_equal(run_transmute(data)$result, c(0.5, 1, 1.5))
})

test_that("transform does not confuse an evaluated result with a value column", {
  data <- data.frame(value = 1:3)

  expect_equal(transform(data, doubled = value * 2)$doubled, c(2, 4, 6))
})
