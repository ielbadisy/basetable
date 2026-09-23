# ---- stack -----------------------------------------------------------------

test_that("stack returns a long table", {
  out <- stack(data.frame(a = 1:2, b = 3:4))

  expect_s3_class(out, "basetable")
  expect_true(all(c("values", "ind") %in% names(out)))
})

# ---- unstack ---------------------------------------------------------------

test_that("unstack reshapes a stacked table", {
  stacked <- data.frame(
    id = c(1, 2),
    var = c("a", "a"),
    value = c(10, 20)
  )

  out <- unstack(stacked, value ~ var)

  expect_s3_class(out, "basetable")
  expect_true("a" %in% names(out))
})

# ---- completegrid ----------------------------------------------------------

test_that("completegrid creates missing combinations", {
  data <- data.frame(site = c("A", "A", "B"), sex = c("M", "F", "M"), value = c(1, 2, 3))

  out <- completegrid(data, cols = c("site", "sex"))

  expect_s3_class(out, "basetable")
  expect_equal(nrow(out), 4L)
  expect_true(any(is.na(out$value)))
})

# ---- expandrows ------------------------------------------------------------

test_that("expandrows repeats rows by count", {
  data <- data.frame(id = c(1, 2), value = c("a", "b"))

  out <- expandrows(data, times = c(2, 1))

  expect_s3_class(out, "basetable")
  expect_equal(out$id, c(1, 1, 2))
  expect_equal(out$value, c("a", "a", "b"))
})
