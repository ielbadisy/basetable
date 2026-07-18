test_that("missingindicator appends logical indicator columns", {
  data <- data.frame(
    id = c(1, 2, 3),
    value = c("a", NA, " ")
  )

  out <- missingindicator(data, cols = "value")

  expect_s3_class(out, "tbl_df")
  expect_true("missing_value" %in% names(out))
  expect_equal(out$missing_value, c(FALSE, TRUE, TRUE))
})
