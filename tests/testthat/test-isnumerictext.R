test_that("isnumerictext detects numeric strings", {
  x <- c("123", "-1.5", ".5", "1e3", "abc", "", NA_character_)

  expect_equal(isnumerictext(x), c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))
})
