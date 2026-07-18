test_that("expandrows repeats rows by count", {
  data <- data.frame(id = c(1, 2), value = c("a", "b"))

  out <- expandrows(data, times = c(2, 1))

  expect_s3_class(out, "tbl_df")
  expect_equal(out$id, c(1, 1, 2))
  expect_equal(out$value, c("a", "a", "b"))
})
