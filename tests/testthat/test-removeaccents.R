test_that("removeaccents transliterates accented text", {
  x <- c("café", "niño")

  expect_equal(removeaccents(x), c("cafe", "nino"))
})
