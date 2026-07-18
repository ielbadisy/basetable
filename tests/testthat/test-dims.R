test_that("dims reports rows and columns", {
  out <- dims(iris)

  expect_s3_class(out, "tbl_df")
  expect_equal(names(out), c("rows", "cols"))
  expect_equal(out$rows[[1]], 150L)
  expect_equal(out$cols[[1]], 5L)
})
