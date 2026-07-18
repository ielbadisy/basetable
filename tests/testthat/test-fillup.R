test_that("fillup carries values backward within groups", {
  data <- data.frame(
    id = c(1, 1, 1, 2, 2),
    visit = c(1, 2, 3, 1, 2),
    treatment = c("A", NA, NA, NA, "B")
  )

  out <- fillup(data, cols = "treatment", by = "id")

  expect_s3_class(out, "tbl_df")
  expect_equal(out$treatment, c("A", NA, NA, "B", "B"))
})
