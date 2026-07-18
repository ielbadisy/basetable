test_that("isalpha detects alphabetic strings", {
  x <- c("abc", "AbC", "abc123", "", NA_character_)

  expect_equal(isalpha(x), c(TRUE, TRUE, FALSE, FALSE, FALSE))
})
