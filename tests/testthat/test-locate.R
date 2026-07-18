test_that("locate returns the first match position", {
  x <- c("ababa", "xyz", NA_character_)

  expect_equal(locate(x, "a"), c(1L, NA_integer_, NA_integer_))
  expect_equal(locate(x, "A", ignore_case = TRUE), c(1L, NA_integer_, NA_integer_))
})
