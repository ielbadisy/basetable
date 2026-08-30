test_that("casewhen picks the first matching label, else default", {
  x <- c(-3, 0, 4, 25)
  expect_equal(
    casewhen(list(neg = x < 0, low = x < 10), default = "high"),
    c("neg", "low", "low", "high")
  )
})

test_that("casewhen default is NA and keeps its type", {
  x <- c(1, 5, 9)
  expect_equal(casewhen(list(big = x > 100)), c(NA, NA, NA))
  expect_identical(casewhen(list(big = x > 100), default = 0L), c(0L, 0L, 0L))
})

test_that("casewhen treats NA in a condition as no match", {
  x <- c(1, NA, 3)
  expect_equal(
    casewhen(list(lo = x < 2, hi = x >= 2), default = "?"),
    c("lo", "?", "hi")
  )
})

test_that("casewhen composes inside transform()", {
  d <- data.frame(v = c(-2, 0, 12))
  out <- transform(d, tier = casewhen(list(neg = v < 0, low = v < 10), default = "high"))
  expect_equal(out$tier, c("neg", "low", "high"))
})

test_that("casewhen validates its input", {
  expect_error(casewhen(list()), "non-empty named list")
  expect_error(casewhen(list(1:3 > 1)), "must be named")
  expect_error(casewhen(list(a = c(TRUE, FALSE), b = c(TRUE, FALSE, TRUE))),
               "same length")
  expect_error(casewhen(list(a = 1:3)), "not a logical vector")
})

test_that("casewhen allows non-syntactic labels via quoted names", {
  age <- c(10, 40, 80)
  expect_equal(
    casewhen(list("under 18" = age < 18, "18-64" = age < 65), default = "65+"),
    c("under 18", "18-64", "65+")
  )
})
