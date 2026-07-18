test_that("assertnames/assertcols/assertkey pass on valid input and throw otherwise", {
  df <- data.frame(a = 1:3, b = c("x", "y", "y"))

  expect_identical(assertnames(df, c("a", "b")), df)
  expect_error(assertnames(df, "nope"), "Missing required names")

  expect_error(assertcols(df, "nope"), "Unknown columns")
  expect_error(assertkey(df, "b"), "does not identify unique rows")
})

test_that("assertrows/invalidrows agree on which rows fail a condition", {
  ok <- data.frame(x = c(1, 2))
  bad <- data.frame(x = c(1, 2, -1, 4))

  invisible_result <- withVisible(assertrows(ok, x > 0))
  expect_true(invisible_result$visible == FALSE)
  expect_identical(invisible_result$value, ok)

  expect_error(assertrows(bad, x > 0), "Some rows failed")
  expect_equal(invalidrows(bad, x > 0)$x, -1)
})

test_that("assertvalues/invalidvalues agree on disallowed values", {
  df <- data.frame(status = c("open", "closed", "bogus", NA))

  expect_error(assertvalues(df, "status", c("open", "closed")), "not allowed")
  expect_equal(invalidvalues(df, "status", c("open", "closed"))$status, "bogus")
})

test_that("assertrange/outofrange agree on out-of-range values", {
  df <- data.frame(x = c(1, 5, 100, NA))

  expect_error(assertrange(df, "x", 0, 10), "out of range")
  expect_equal(outofrange(df, "x", 0, 10)$x, 100)
})

test_that("asserttype checks class", {
  df <- data.frame(x = 1:3, y = c("a", "b", "c"), stringsAsFactors = FALSE)

  expect_identical(asserttype(df, "x", "integer"), df)
  expect_error(asserttype(df, "y", "integer"), "Unexpected type")
})

test_that("assertunique checks uniqueness of the selected columns", {
  df <- data.frame(id = c(1, 2, 2))

  expect_error(assertunique(df, "id"), "not unique")
  expect_identical(assertunique(data.frame(id = c(1, 2, 3)), "id"), data.frame(id = c(1, 2, 3)))
})

test_that("assertcomplete checks for missing values", {
  df <- data.frame(x = c(1, NA), y = c("a", "b"))

  expect_error(assertcomplete(df), "Missing values found")
  expect_identical(assertcomplete(df, cols = "y"), df)
})
