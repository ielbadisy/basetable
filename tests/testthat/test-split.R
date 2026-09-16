test_that("split returns a list of table pieces", {
  out <- split(iris, by = "Species")

  expect_type(out, "list")
  expect_equal(length(out), 3L)
  expect_s3_class(out[[1]], "basetable")
})

test_that("split falls back to base::split() for non-data-frame input", {
  x <- 1:10
  f <- rep(1:2, 5)

  expect_equal(split(x, f), base::split(x, f))
  expect_equal(split(letters[1:6], rep(c("a", "b"), 3)),
               base::split(letters[1:6], rep(c("a", "b"), 3)))
})
