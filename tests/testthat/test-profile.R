test_that("profile delegates to describe", {
  out <- profile(iris)

  expect_s3_class(out, "basetable")
  expect_equal(nrow(out), ncol(iris))
  expect_true(all(c("column", "class", "distinct") %in% names(out)))
})
