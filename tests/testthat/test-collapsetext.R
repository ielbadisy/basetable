test_that("collapsetext collapses text into one string", {
  x <- c("a", "b", "c")

  expect_equal(collapsetext(x, sep = "-"), "a-b-c")
})
