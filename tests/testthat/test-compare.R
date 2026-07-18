test_that("compare returns a named list of tables", {
  out <- compare(iris, iris, by = "Species")

  expect_type(out, "list")
  expect_true(all(c("dims", "names", "types", "missing", "key_overlap") %in% names(out)))
  expect_s3_class(out$dims, "tbl_df")
})
