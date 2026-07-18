test_that("isemail detects email-like strings", {
  x <- c("a@b.com", "name@example.co.uk", "bad@", "bad", NA_character_)

  expect_equal(isemail(x), c(TRUE, TRUE, FALSE, FALSE, FALSE))
})
