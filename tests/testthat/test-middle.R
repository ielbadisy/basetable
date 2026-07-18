test_that("middle takes a substring from the middle", {
  x <- c("abcdef", "xyz")

  expect_equal(middle(x, 2, 4), c("bcd", "yz"))
})
