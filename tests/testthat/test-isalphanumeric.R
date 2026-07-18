test_that("isalphanumeric detects alphanumeric strings", {
  x <- c("abc", "abc123", "abc-123", "", NA_character_)

  expect_equal(isalphanumeric(x), c(TRUE, TRUE, FALSE, FALSE, FALSE))
})
