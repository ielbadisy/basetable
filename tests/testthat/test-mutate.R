test_that("mutate adds derived columns", {
  out <- mutate(mtcars, ratio = mpg / cyl)

  expect_s3_class(out, "data.table")
  expect_true("ratio" %in% names(out))
  expect_equal(out$ratio[[1]], mtcars$mpg[[1]] / mtcars$cyl[[1]])
})
