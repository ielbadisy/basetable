# ---- misc helpers ----------------------------------------------------------

test_that("applycols/convertcols/replacecols/replacewhere transform selected columns", {
  df <- data.frame(a = 1:3, b = 4:6)

  out <- applycols(df, "a", function(x) x * 10)
  expect_equal(out$a, c(10, 20, 30))
  expect_equal(out$b, df$b)

  out2 <- convertcols(df, "a", as.character)
  expect_equal(out2$a, c("1", "2", "3"))

  out3 <- replacecols(df, c("a", "b"), list(c(9, 9, 9), c(8, 8, 8)))
  expect_equal(out3$a, c(9, 9, 9))
  expect_equal(out3$b, c(8, 8, 8))

  out4 <- replacewhere(df, a > 1, "b", 0)
  expect_equal(out4$b, c(4, 0, 0))
})

test_that("applyby applies a function per group and can bind the results back", {
  df <- data.frame(g = c("a", "a", "b"), v = c(1, 2, 3))

  out <- applyby(df, "g", nrow)
  expect_equal(unname(unlist(out)), c(2, 1))

  bound <- applyby(df, "g", function(d) data.frame(n = nrow(d)), bind = TRUE, id = "src")
  expect_equal(nrow(bound), 2L)
  expect_true("src" %in% names(bound))
})

test_that("naif/nato/blanktona/natoblank/replacevalues recode values", {
  expect_equal(naif(c(1, 99, 2), 99), c(1, NA, 2))
  expect_equal(nato(c(1, NA, 2), 0), c(1, 0, 2))
  expect_equal(blanktona(c("a", "", " ", "b")), c("a", NA, NA, "b"))
  expect_equal(natoblank(c("a", NA, "b")), c("a", "", "b"))
  expect_equal(replacevalues(c("a", "b", "c"), c("a", "b"), c("A", "B")), c("A", "B", "c"))
})

test_that("tolong/towide/transpose reshape a table", {
  wide <- data.frame(id = 1:2, x = c(10, 20), y = c(30, 40))

  long <- tolong(wide, cols = c("x", "y"))
  expect_equal(nrow(long), 4L)
  expect_true(all(c("id", "variable", "value") %in% names(long)))

  back <- towide(long, names = "variable", values = "value", idcols = "id", fun = sum)
  expect_equal(sort(names(back)), sort(c("id", "x", "y")))
  expect_equal(back$x, wide$x)
  expect_equal(back$y, wide$y)

  t_out <- transpose(data.frame(a = 1:2, b = 3:4))
  expect_equal(dim(t_out), c(2L, 2L))
})

test_that("map/traverse/fold/orderrows work as expected", {
  expect_equal(map(1:3, function(x) x + 1), list(2, 3, 4))
  expect_equal(traverse(list(a = 1:2, b = 10:11), function(a, b) a + b), list(11, 13))
  expect_equal(foldr(1:4, `+`), 10L)

  df <- data.frame(x = c(3, 1, 2))
  expect_equal(orderrows(df, "x")$x, c(1, 2, 3))
})

# ---- compact ---------------------------------------------------------------

test_that("compact drops only NULL elements", {
  out <- compact(list(1, NULL, 2, NULL, 3))
  expect_equal(out, list(1, 2, 3))
})

test_that("compact keeps falsy-but-not-NULL values", {
  out <- compact(list(a = 1, b = NULL, c = NA, d = 0, e = ""))
  expect_equal(out, list(a = 1, c = NA, d = 0, e = ""))
})

test_that("compact preserves names and order", {
  out <- compact(list(x = 1, y = NULL, z = 2))
  expect_equal(names(out), c("x", "z"))
})

test_that("compact returns an empty list unchanged", {
  expect_equal(compact(list()), list())
})

test_that("compact errors on non-list input", {
  expect_error(compact(c(1, 2, 3)), "must be a list")
})

# ---- casewhen --------------------------------------------------------------

test_that("casewhen picks the first matching label, else default", {
  x <- c(-3, 0, 4, 25)
  expect_equal(
    casewhen(list(neg = x < 0, low = x < 10), default = "high"),
    c("neg", "low", "low", "high")
  )
})

test_that("casewhen default is NA and keeps its type", {
  x <- c(1, 5, 9)
  expect_equal(casewhen(list(big = x > 100)), c(NA, NA, NA))
  expect_identical(casewhen(list(big = x > 100), default = 0L), c(0L, 0L, 0L))
})

test_that("casewhen treats NA in a condition as no match", {
  x <- c(1, NA, 3)
  expect_equal(
    casewhen(list(lo = x < 2, hi = x >= 2), default = "?"),
    c("lo", "?", "hi")
  )
})

test_that("casewhen composes inside transform()", {
  d <- data.frame(v = c(-2, 0, 12))
  out <- transform(d, tier = casewhen(list(neg = v < 0, low = v < 10), default = "high"))
  expect_equal(out$tier, c("neg", "low", "high"))
})

test_that("casewhen validates its input", {
  expect_error(casewhen(list()), "non-empty named list")
  expect_error(casewhen(list(1:3 > 1)), "must be named")
  expect_error(casewhen(list(a = c(TRUE, FALSE), b = c(TRUE, FALSE, TRUE))),
               "same length")
  expect_error(casewhen(list(a = 1:3)), "not a logical vector")
})

test_that("casewhen allows non-syntactic labels via quoted names", {
  age <- c(10, 40, 80)
  expect_equal(
    casewhen(list("under 18" = age < 18, "18-64" = age < 65), default = "65+"),
    c("under 18", "18-64", "65+")
  )
})

# ---- parsing helpers -------------------------------------------------------

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

# ---- assertion helpers -----------------------------------------------------

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

# ---- set schema helpers ----------------------------------------------------

test_that("equaldata compares full table content", {
  x <- data.frame(a = 1:3, b = c("x", "y", "z"), stringsAsFactors = FALSE)
  y <- data.frame(a = 3:1, b = c("z", "y", "x"), stringsAsFactors = FALSE)

  expect_false(equaldata(x, y))
  expect_true(equaldata(x, y, ignoreorder = TRUE))
})

test_that("equalrows compares rows by key, ignoring order but not duplicates", {
  x <- data.frame(id = c(1, 2, 3))
  y_reordered <- data.frame(id = c(3, 2, 1))
  y_extra_dup <- data.frame(id = c(1, 2, 3, 3))
  y_different <- data.frame(id = c(4, 5, 6))

  expect_true(equalrows(x, y_reordered, by = "id"))
  expect_false(equalrows(x, y_extra_dup, by = "id"))
  expect_false(equalrows(x, y_different, by = "id"))
})

test_that("equalrows compares full row content, not just the key columns", {
  same_v <- data.frame(id = 1:3, v = c("a", "b", "c"))
  different_v <- data.frame(id = 1:3, v = c("a", "b", "ZZZZZ"))

  expect_true(equalrows(same_v, same_v, by = "id"))
  expect_false(equalrows(same_v, different_v, by = "id"))
})

test_that("sameschema/compareschema report column name and type differences", {
  x <- data.frame(a = 1L, b = "x", stringsAsFactors = FALSE)
  y <- data.frame(a = 1L, b = "x", stringsAsFactors = FALSE)
  z <- data.frame(a = 1L, c = 2, stringsAsFactors = FALSE)

  expect_true(sameschema(x, y))
  expect_false(sameschema(x, z))

  out <- compareschema(x, z)
  expect_equal(sort(out$column), c("a", "b", "c"))
  expect_false(out$in_y[out$column == "b"])
  expect_false(out$in_x[out$column == "c"])

  expect_equal(changedcols(x, z), compareschema(x, z))
})

test_that("joinrelationship classifies key cardinality", {
  one <- data.frame(id = c(1, 2, 3))
  dup <- data.frame(id = c(1, 1, 2))

  expect_equal(joinrelationship(one, one, "id"), "one-to-one")
  expect_equal(joinrelationship(one, dup, "id"), "one-to-many")
  expect_equal(joinrelationship(dup, one, "id"), "many-to-one")
  expect_equal(joinrelationship(dup, dup, "id"), "many-to-many")
})

test_that("unionrows/rbindfill combine rows", {
  x <- data.frame(id = c(1, 2))
  y <- data.frame(id = c(2, 3))

  out <- unionrows(x, y)
  expect_equal(sort(out$id), c(1, 2, 3))

  out_fill <- rbindfill(data.frame(a = 1), data.frame(a = 2, b = 3))
  expect_equal(out_fill$a, c(1, 2))
  expect_equal(out_fill$b, c(NA, 3))
})

test_that("rbindfill(typeconflict =) controls handling of mismatched column types", {
  a <- data.frame(id = 1, v = "1", stringsAsFactors = FALSE)
  b <- data.frame(id = 2, v = 2L)

  expect_error(rbindfill(a, b), "conflicting types")
  expect_error(rbindfill(a, b, typeconflict = "error"), "conflicting types")

  out <- rbindfill(a, b, typeconflict = "coerce")
  expect_equal(out$v, c("1", "2"))

  same_type <- rbindfill(data.frame(v = 1), data.frame(v = 2))
  expect_equal(same_type$v, c(1, 2))
})

test_that("addedrows/removedrows/changedrows track differences between two snapshots", {
  old <- data.frame(id = c(1, 2, 3), v = c("a", "b", "c"), stringsAsFactors = FALSE)
  new <- data.frame(id = c(2, 3, 4), v = c("b", "c2", "d"), stringsAsFactors = FALSE)

  expect_equal(addedrows(old, new, by = "id")$id, 4)
  expect_equal(removedrows(old, new, by = "id")$id, 1)

  changed <- changedrows(old, new, by = "id")
  expect_equal(changed$id, 3)
})

# ---- errors ----------------------------------------------------------------

test_that("missing columns are rejected clearly", {
  expect_error(pick(mtcars, "nope"), "Unknown columns")
  expect_error(assert_cols(mtcars, "nope"), "Unknown columns")
})

test_that("duplicated keys are reported", {
  x <- data.frame(id = c(1, 1, 2), value = c("a", "b", "c"))
  dup <- duplicated_keys(x, "id")
  expect_equal(dup$id, 1)
  expect_error(assert_key(x, "id"), "does not identify unique rows")
})

test_that("summarytab validates p-value and stratification inputs", {
  expect_error(summarytab(mtcars, vars = "mpg", p_value = TRUE), "requires `by`")
  expect_error(summarytab(mtcars, vars = "am", by = "am"), "must not include")
  expect_error(summarytab(mtcars, vars = character(0), by = "am"), "at least one column")
})
