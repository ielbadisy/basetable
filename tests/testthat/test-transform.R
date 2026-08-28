test_that("transform adds derived columns", {
  out <- transform(mtcars, ratio = mpg / cyl)

  expect_s3_class(out, "data.table")
  expect_true("ratio" %in% names(out))
  expect_equal(out$ratio[[1]], mtcars$mpg[[1]] / mtcars$cyl[[1]])
})

test_that("transform() with .keep = FALSE keeps only new columns", {
  out <- transform(mtcars, ratio = mpg / cyl, .keep = FALSE)

  expect_s3_class(out, "data.table")
  expect_equal(names(out), "ratio")
})

test_that("transform resolves values from its caller, including nested and .keep = FALSE calls", {
  data <- data.frame(value = 1:3)

  run_transform <- function(x) {
    multiplier <- 3
    transform(x, result = value * multiplier)
  }
  run_transform_keep <- function(x) {
    offset <- 2
    transform(x, result = value + offset)
  }
  run_transform_drop <- function(x) {
    denominator <- 2
    transform(x, result = value / denominator, .keep = FALSE)
  }

  expect_equal(run_transform(data)$result, c(3, 6, 9))
  expect_equal(run_transform_keep(data)$result, c(3, 4, 5))
  expect_equal(run_transform_drop(data)$result, c(0.5, 1, 1.5))
})

test_that("transform does not confuse an evaluated result with a value column", {
  data <- data.frame(value = 1:3)

  expect_equal(transform(data, doubled = value * 2)$doubled, c(2, 4, 6))
})

test_that("transform(by =) computes expressions within each group", {
  data <- data.frame(g = c("a", "a", "b", "b"), x = c(1, 2, 3, 4))

  out <- transform(data, cumx = cumsum(x), by = "g")
  expect_equal(out$cumx, c(1, 3, 3, 7))

  dev <- transform(data, dev = x - mean(x), by = "g")
  expect_equal(dev$dev, c(-0.5, 0.5, -0.5, 0.5))
})

test_that("transform(by =) resolves external variables from its caller", {
  data <- data.frame(g = c("a", "a", "b", "b"), x = c(1, 2, 3, 4))

  run <- function(x) {
    offset <- 10
    transform(x, y = x + offset, by = "g")
  }

  expect_equal(run(data)$y, data$x + 10)
})

test_that("transform(by =) does not mutate its input", {
  input <- data.frame(g = c("a", "a", "b"), x = c(1, 2, 3))
  original <- input

  transform(input, cumx = cumsum(x), by = "g")

  expect_identical(input, original)
})
