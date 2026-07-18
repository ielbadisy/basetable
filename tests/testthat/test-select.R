test_that("select returns selected columns", {
  out <- select(mtcars, c("mpg", "cyl"))

  expect_s3_class(out, "tbl_df")
  expect_equal(names(out), c("mpg", "cyl"))
})
