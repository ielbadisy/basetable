test_that("unstack reshapes a stacked table", {
  stacked <- data.frame(
    id = c(1, 2),
    var = c("a", "a"),
    value = c(10, 20)
  )

  out <- unstack(stacked, value ~ var)

  expect_s3_class(out, "tbl_df")
  expect_true("a" %in% names(out))
})
