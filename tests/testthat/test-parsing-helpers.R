test_that("parseint parses integers and honors na=/strict=", {
  expect_equal(parseint(c("1", "2", "NA")), c(1L, 2L, NA_integer_))
  expect_equal(parseint(c("1", "missing"), na = "missing"), c(1L, NA_integer_))
  expect_error(parseint("abc", strict = TRUE), "Parse failure")
  expect_equal(parseint("abc", strict = FALSE), NA_integer_)
})

test_that("parsenum handles decimal/grouping marks and na=/strict=", {
  expect_equal(parsenum("1,234.5"), 1234.5)
  expect_equal(parsenum("1.234,5", decimal = ",", grouping = "."), 1234.5)
  expect_equal(parsenum(c("1", "missing"), na = "missing"), c(1, NA_real_))
  expect_error(parsenum("abc", strict = TRUE), "Parse failure")
})

test_that("parselogical maps common true/false spellings", {
  expect_equal(parselogical(c("TRUE", "false", "T", "0")), c(TRUE, FALSE, TRUE, FALSE))
  expect_equal(parselogical(c("1", "missing"), na = "missing"), c(TRUE, NA))
  expect_error(parselogical("yes", strict = TRUE), "Parse failure")
})

test_that("parsedate tries multiple formats in order", {
  x <- c("2024-01-15", "15/01/2024")
  out <- parsedate(x, formats = c("%Y-%m-%d", "%d/%m/%Y"))
  expect_equal(out, as.Date(c("2024-01-15", "2024-01-15")))
  expect_error(parsedate("not-a-date", formats = "%Y-%m-%d", strict = TRUE), "Parse failure")
})

test_that("parsedatetime tries multiple formats in order", {
  x <- "2024-01-15 08:30:00"
  out <- parsedatetime(x, formats = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  expect_equal(out, as.POSIXct("2024-01-15 08:30:00", tz = "UTC"))
})

test_that("parsepercent/parsecurrency strip their marker and parse", {
  expect_equal(parsepercent("42%"), 0.42)
  expect_equal(parsecurrency("$1,234.50"), 1234.50)
})

test_that("parsefailures reports the rows that failed to parse", {
  out <- parsefailures(c("1", "x", "3"), parseint)
  expect_equal(out$index, 2L)
  expect_equal(out$value, "x")
})
