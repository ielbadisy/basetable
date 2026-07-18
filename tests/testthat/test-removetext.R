test_that("removetext removes the first literal match", {
  x <- c("abcabc", "xyz", NA_character_)

  expect_equal(removetext(x, "ab"), c("cabc", "xyz", NA_character_))
})
