test_that("rowmin and rowmax handle NA correctly", {
  df <- data.frame(a = c(1, NA, 3), b = c(2, 2, NA), c = c(3, 1, 1))
  expect_equal(rowmin(df, na.rm = TRUE), c(1, 1, 1))
  expect_equal(rowmax(df, na.rm = TRUE), c(3, 2, 3))
  expect_equal(rowmin(df, na.rm = FALSE), c(1, NA, NA))
  expect_equal(rowmax(df, na.rm = FALSE), c(3, NA, NA))
})

test_that("rowany and rowall handle NA correctly", {
  df <- data.frame(x = c(TRUE, FALSE, NA, TRUE, NA), y = c(FALSE, FALSE, NA, NA, TRUE))
  expect_equal(rowany(df, na.rm = TRUE), c(TRUE, FALSE, FALSE, TRUE, TRUE))
  expect_equal(rowany(df, na.rm = FALSE), c(TRUE, FALSE, NA, TRUE, TRUE))
  expect_equal(rowall(df, na.rm = TRUE), c(FALSE, FALSE, TRUE, TRUE, TRUE))
  expect_equal(rowall(df, na.rm = FALSE), c(FALSE, FALSE, NA, NA, NA))
})

test_that("rowcount counts matches per row", {
  df <- data.frame(a = c(1, NA, 3), b = c(3, 2, 3), c = c(3, 3, NA))
  expect_equal(rowcount(df, value = 3, na.rm = TRUE), c(2L, 1L, 2L))
  expect_equal(rowcount(df, value = 3, na.rm = FALSE), c(2L, NA_integer_, NA_integer_))
})

test_that("rowfirst and rowlast respect na.rm", {
  df <- data.frame(a = c(1, NA, NA), b = c(NA, 2, NA), c = c(3, 3, NA))
  expect_equal(rowfirst(df, na.rm = TRUE), c(1, 2, NA))
  expect_equal(rowfirst(df, na.rm = FALSE), c(1, NA, NA))
  expect_equal(rowlast(df, na.rm = TRUE), c(3, 3, NA))
  expect_equal(rowlast(df, na.rm = FALSE), c(3, 3, NA))
})
