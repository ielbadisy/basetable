test_that("isintegertext detects integer strings", {
  x <- c("123", "-1", "+42", "1.5", "abc", "", NA_character_)

  expect_equal(isintegertext(x), c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE))
})
