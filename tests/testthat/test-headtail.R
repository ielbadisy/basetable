test_that("headtail returns the first and last rows", {
  out <- headtail(iris, n = 2)

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 4L)
  expect_equal(out$Sepal.Length[[1]], iris$Sepal.Length[[1]])
  expect_equal(out$Sepal.Length[[4]], iris$Sepal.Length[[150]])
})
