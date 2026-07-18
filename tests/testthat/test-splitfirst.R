test_that("splitfirst returns the first piece", {
  x <- c("a,b,c", "x,y", NA_character_)

  expect_equal(splitfirst(x, ","), c("a", "x", NA_character_))
})
