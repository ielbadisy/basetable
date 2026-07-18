test_that("nearesttext returns the closest candidate", {
  x <- c("catt", "batt", NA_character_)
  candidates <- c("cat", "bat", "rat")

  expect_equal(nearesttext(x, candidates), c("cat", "bat", NA_character_))
})
