test_that("distinct returns unique rows", {
  out <- distinct(mtcars, cols = "cyl")

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), length(unique(mtcars$cyl)))
})
