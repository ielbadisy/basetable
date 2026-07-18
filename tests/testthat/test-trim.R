test_that("trim removes leading and trailing whitespace", {
  x <- c("  a ", "\tb\t", "c")

  expect_equal(trim(x), c("a", "b", "c"))
})
