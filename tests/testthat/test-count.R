test_that("count returns one row per group", {
  out <- count(iris, by = "Species")

  expect_s3_class(out, "tbl_df")
  expect_true(all(c("Species", "n") %in% names(out)))
  expect_equal(sum(out$n), nrow(iris))
})
