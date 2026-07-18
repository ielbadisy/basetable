test_that("countmatch counts regex matches", {
  x <- c("ababa", "xyz", NA_character_)

  expect_equal(countmatch(x, "a"), c(3L, 0L, NA_integer_))
  expect_equal(countmatch(x, "A", ignore_case = TRUE), c(3L, 0L, NA_integer_))
})
