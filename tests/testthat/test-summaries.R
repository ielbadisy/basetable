test_that("summaries returns named summaries", {
  out <- summaries(mtcars, by = "cyl", mean_mpg = mean(mpg), n = length(mpg))

  expect_s3_class(out, "tbl_df")
  expect_true(all(c("cyl", "mean_mpg", "n") %in% names(out)))
  expect_equal(nrow(out), length(unique(mtcars$cyl)))
})
