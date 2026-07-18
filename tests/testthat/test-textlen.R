test_that("textlen counts characters", {
  x <- c("abc", "", "hello")

  expect_equal(textlen(x), c(3L, 0L, 5L))
})
