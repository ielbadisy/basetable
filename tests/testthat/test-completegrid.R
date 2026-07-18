test_that("completegrid creates missing combinations", {
  data <- data.frame(site = c("A", "A", "B"), sex = c("M", "F", "M"), value = c(1, 2, 3))

  out <- completegrid(data, cols = c("site", "sex"))

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 4L)
  expect_true(any(is.na(out$value)))
})
