test_that("rangemerge returns a tibble for explicit range inputs", {
  x <- data.frame(id = c(1, 2), lower = c(10, 20), upper = c(15, 25))
  y <- data.frame(id = c(1, 2), value = c("a", "b"))

  out <- rangemerge(x, y, by = "id", lower = "lower", upper = "upper")

  expect_s3_class(out, "tbl_df")
  expect_true("id" %in% names(out))
})
