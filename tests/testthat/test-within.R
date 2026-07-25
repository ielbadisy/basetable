test_that("within adds new columns", {
  out <- within(iris, ratio <- Sepal.Length / Sepal.Width)

  expect_s3_class(out, "data.table")
  expect_true("ratio" %in% names(out))
})
