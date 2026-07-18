test_that("rename changes column names", {
  out <- rename(mtcars, miles = mpg, cylinders = cyl)

  expect_s3_class(out, "tbl_df")
  expect_true(all(c("miles", "cylinders") %in% names(out)))
})
