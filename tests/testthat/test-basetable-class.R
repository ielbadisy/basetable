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

test_that("as_basetable() coerces frames, lists and matrices without changing data", {
  bt <- as_basetable(data.frame(g = c("a", "b"), x = 1:2))
  expect_s3_class(bt, "basetable")
  expect_s3_class(bt, "data.frame")
  expect_false(inherits(bt, "data.table"))
  expect_identical(as.data.frame(bt), data.frame(g = c("a", "b"), x = 1:2))

  from_list <- as_basetable(list(g = c("a", "b"), x = 1:2))
  expect_s3_class(from_list, "basetable")
  expect_equal(from_list$x, 1:2)

  from_mat <- as_basetable(matrix(1:4, 2, dimnames = list(NULL, c("a", "b"))))
  expect_s3_class(from_mat, "basetable")
  expect_equal(from_mat$b, c(3L, 4L))

  expect_equal(nrow(as_basetable(data.frame())), 0L)
})

test_that("as_basetable() is a no-op on a basetable and is inverted by as.data.frame()", {
  bt <- as_basetable(mtcars)
  expect_identical(as_basetable(bt), bt)
  expect_identical(class(as.data.frame(bt)), "data.frame")
})

test_that("is_basetable() recognises the class", {
  expect_true(is_basetable(as_basetable(mtcars)))
  expect_false(is_basetable(mtcars))
  expect_false(is_basetable(list(a = 1)))
})
