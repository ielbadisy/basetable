test_that("rescale/standardize/center/winsorize", {
  x <- c(1, 2, 3, 4, 5)

  expect_equal(rescale(x), c(0, 0.25, 0.5, 0.75, 1))
  expect_equal(rescale(x, to = c(0, 10)), c(0, 2.5, 5, 7.5, 10))

  expect_equal(standardize(x), (x - mean(x)) / sd(x))
  expect_equal(center(x), x - mean(x))

  y <- c(1, 2, 3, 4, 100)
  out <- winsorize(y, probs = c(0.1, 0.9))
  expect_true(max(out) < 100)
  expect_equal(out[2:3], y[2:3])
})

test_that("quantilegroup bins into the requested number of groups", {
  x <- 1:100
  out <- quantilegroup(x, n = 4)
  expect_equal(nlevels(out), 4L)
})

test_that("percentchange/percentrank", {
  expect_equal(percentchange(c(100, 110, 99)), c(NA, 10, -10))

  x <- c(10, 20, 20, 30)
  out <- percentrank(x)
  expect_equal(out[1], 1 / 4)
  expect_equal(out[4], 1)
})

test_that("cumcount/cumedist/cummean", {
  expect_equal(cumcount(c("a", "b", "c")), 1:3)
  expect_equal(cumedist(c("a", "a", "b")), cumsum(!duplicated(c("a", "a", "b"))) / 1:3)
  expect_equal(cummean(c(1, 2, 3)), c(1, 1.5, 2))
})

test_that("difference/lagvalue/leadvalue", {
  expect_equal(difference(c(1, 3, 6)), c(NA, 2, 3))
  expect_equal(difference(c(1, 3, 6, 10), lag = 2), c(NA, NA, 5, 7))

  expect_equal(lagvalue(1:4), c(NA, 1, 2, 3))
  expect_equal(lagvalue(1:4, n = 2, default = 0), c(0, 0, 1, 2))
  expect_equal(leadvalue(1:4), c(2, 3, 4, NA))
})

test_that("propcount computes proportions overall and within a margin", {
  df <- data.frame(grp = c("a", "a", "b"), sub = c("x", "y", "x"), stringsAsFactors = FALSE)

  out <- propcount(df, by = "grp")
  expect_equal(sum(out$prop), 1)

  out_margin <- propcount(df, by = c("grp", "sub"), margin = "grp")
  a_rows <- out_margin[out_margin$grp == "a", ]
  expect_equal(sum(a_rows$prop), 1)
})
