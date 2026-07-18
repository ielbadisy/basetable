test_that("stack returns a long table", {
  out <- stack(data.frame(a = 1:2, b = 3:4))

  expect_s3_class(out, "tbl_df")
  expect_true(all(c("values", "ind") %in% names(out)))
})
