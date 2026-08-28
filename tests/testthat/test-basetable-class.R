test_that("verbs return a basetable, not a data.table", {
  out <- pick(mtcars, c("mpg", "cyl"))
  expect_s3_class(out, "basetable")
  expect_s3_class(out, "data.frame")
  expect_false(inherits(out, "data.table"))
  expect_identical(class(as.data.frame(out)), "data.frame")
})

test_that("[.basetable keeps the class and does not drop by default", {
  out <- pick(mtcars, c("mpg", "cyl", "disp"))

  one_col <- out[, "mpg"]
  expect_s3_class(one_col, "basetable")
  expect_equal(ncol(one_col), 1L)

  rows <- out[1:3, ]
  expect_s3_class(rows, "basetable")
  expect_equal(nrow(rows), 3L)

  expect_type(out[["mpg"]], "double")
})

test_that("print.basetable is compact and returns its input invisibly", {
  out <- pick(mtcars, "mpg")
  txt <- capture.output(res <- withVisible(print(out)))
  expect_false(res$visible)
  expect_identical(res$value, out)
  expect_match(txt[[1]], "^# basetable: 32 x 1")
  expect_true(any(grepl("more rows", txt)))
})

test_that("basetable survives a round-trip through as.data.frame / as.list", {
  out <- aggregate(mtcars, by = "cyl", value = "mpg", fun = mean)
  df <- as.data.frame(out)
  expect_identical(class(df), "data.frame")
  expect_equal(df$mpg, out$mpg)
  expect_equal(as.list(out)$mpg, out$mpg)
})
