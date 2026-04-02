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
