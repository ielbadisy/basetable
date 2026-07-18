test_that("arrange sorts rows", {
  out <- arrange(mtcars, by = "mpg")

  expect_s3_class(out, "tbl_df")
  expect_false(is.unsorted(out$mpg, strictly = FALSE))
})
