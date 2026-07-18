test_that("missingrows returns rows with missing values", {
  data <- data.frame(
    id = c(1, 2, 3),
    value = c("a", NA, " ")
  )

  out_any <- missingrows(data)
  out_all <- missingrows(data, cols = "value", mode = "all")

  expect_s3_class(out_any, "tbl_df")
  expect_equal(out_any$id, c(2, 3))
  expect_equal(out_all$id, c(2, 3))
})
