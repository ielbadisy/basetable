test_that("isblank detects missing and blank values", {
  x <- c("a", "", " ", NA_character_)

  expect_equal(isblank(x), c(FALSE, TRUE, TRUE, TRUE))
})
