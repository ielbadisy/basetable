test_that("within adds new columns", {
  out <- within(iris, ratio <- Sepal.Length / Sepal.Width)

  expect_s3_class(out, "tbl_df")
  expect_true("ratio" %in% names(out))
})
