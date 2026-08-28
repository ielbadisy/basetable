test_that("describe returns one row per column", {
  out <- describe(iris)

  expect_s3_class(out, "basetable")
  expect_equal(nrow(out), ncol(iris))
  expect_true(all(c("column", "class", "distinct") %in% names(out)))
})
