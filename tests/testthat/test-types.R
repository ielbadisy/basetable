test_that("types reports one row per column", {
  out <- types(iris)

  expect_s3_class(out, "tbl_df")
  expect_equal(names(out), c("column", "class", "typeof"))
  expect_equal(nrow(out), ncol(iris))
  expect_true(all(c("Sepal.Length", "Species") %in% out$column))
})
