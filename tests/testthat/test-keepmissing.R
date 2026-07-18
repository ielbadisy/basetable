test_that("keepmissing returns rows with missing values", {
  data <- data.frame(
    id = c(1, 2, 3),
    value = c("a", NA, " ")
  )

  out <- keepmissing(data)

  expect_s3_class(out, "tbl_df")
  expect_equal(out$id, c(2, 3))
})
