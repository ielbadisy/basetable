test_that("dims, types, describe, and missing return data frames", {
  expect_s3_class(dims(iris), "data.frame")
  expect_s3_class(types(iris), "data.frame")
  expect_s3_class(describe(iris), "data.frame")
  expect_s3_class(missingness(iris), "data.frame")
  expect_s3_class(missingness(iris, margin = "row"), "data.frame")
})

test_that("freq and compare provide stable outputs", {
  out <- freq(iris, column = "Species", prop = TRUE)
  expect_true(all(c("Species", "n", "prop") %in% names(out)))

  cmp <- compare(
    iris,
    transform(iris, Sepal.Width = Sepal.Width + 1),
    by = "Species"
  )
  expect_true(all(c("dims", "names", "types", "missing", "key_overlap") %in% names(cmp)))
})
