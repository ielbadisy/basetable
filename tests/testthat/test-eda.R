test_that("dims, types, describe, missingness, and summarytab return data frames", {
  expect_s3_class(dims(iris), "data.frame")
  expect_s3_class(types(iris), "data.frame")
  expect_s3_class(describe(iris), "data.frame")
  expect_s3_class(missingness(iris), "data.frame")
  expect_s3_class(missingness(iris, margin = "row"), "data.frame")
  expect_s3_class(summarytab(iris, vars = c("Sepal.Length", "Species")), "data.frame")
})

test_that("missingness treats blank strings as missing, consistent with missingrows()/missingindicator()", {
  df <- data.frame(a = c(1, NA, 3, NA), b = c("", "y", NA, "z"), stringsAsFactors = FALSE)

  out <- missingness(df)
  expect_equal(out$missing[out$column == "a"], 2)
  expect_equal(out$missing[out$column == "b"], 2)

  out_row <- missingness(df, margin = "row")
  expect_equal(out_row$missing, c(1, 1, 1, 1))
  expect_equal(out_row$complete, rep(FALSE, 4))
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

test_that("summarytab builds table1-style summaries with optional p-values", {
  dat <- transform(
    mtcars,
    am = factor(am, levels = c(0, 1), labels = c("Automatic", "Manual")),
    cyl = factor(cyl)
  )

  out <- summarytab(dat, vars = c("mpg", "cyl"), by = "am", p_value = TRUE)

  expect_equal(
    names(out),
    c("variable", "level", "Automatic", "Manual", "Overall", "p_value")
  )
  expect_equal(out$level[[1]], "Mean (SD)")
  expect_true(any(out$variable == "mpg"))
  expect_true(any(out$variable == "cyl"))
  expect_true(any(!is.na(out$p_value) & nzchar(out$p_value)))
})
