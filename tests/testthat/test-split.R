test_that("split returns a list of table pieces", {
  out <- split(iris, by = "Species")

  expect_type(out, "list")
  expect_equal(length(out), 3L)
  expect_s3_class(out[[1]], "tbl_df")
})
