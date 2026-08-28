test_that("freq counts values", {
  out <- freq(iris, column = "Species")

  expect_s3_class(out, "basetable")
  expect_true(all(c("Species", "n") %in% names(out)))
  expect_equal(sum(out$n), nrow(iris))
})
