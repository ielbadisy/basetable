# ---- basetable class -------------------------------------------------------

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

test_that("asbasetable() coerces frames, lists and matrices without changing data", {
  bt <- asbasetable(data.frame(g = c("a", "b"), x = 1:2))
  expect_s3_class(bt, "basetable")
  expect_s3_class(bt, "data.frame")
  expect_false(inherits(bt, "data.table"))
  expect_identical(as.data.frame(bt), data.frame(g = c("a", "b"), x = 1:2))

  from_list <- asbasetable(list(g = c("a", "b"), x = 1:2))
  expect_s3_class(from_list, "basetable")
  expect_equal(from_list$x, 1:2)

  from_mat <- asbasetable(matrix(1:4, 2, dimnames = list(NULL, c("a", "b"))))
  expect_s3_class(from_mat, "basetable")
  expect_equal(from_mat$b, c(3L, 4L))

  expect_equal(nrow(asbasetable(data.frame())), 0L)
})

test_that("asbasetable() is a no-op on a basetable and is inverted by as.data.frame()", {
  bt <- asbasetable(mtcars)
  expect_identical(asbasetable(bt), bt)
  expect_identical(class(as.data.frame(bt)), "data.frame")
})

test_that("isbasetable() recognises the class", {
  expect_true(isbasetable(asbasetable(mtcars)))
  expect_false(isbasetable(mtcars))
  expect_false(isbasetable(list(a = 1)))
})

test_that("asbasetable() returns a plain basetable for a subclass", {
  bt <- asbasetable(data.frame(x = 1:2))
  expect_identical(asbasetable(bt), bt)
  sub <- structure(bt, class = c("myclass", "basetable", "data.frame"))
  out <- asbasetable(sub)
  expect_identical(class(out), c("basetable", "data.frame"))
  expect_identical(out$x, 1:2)
})
