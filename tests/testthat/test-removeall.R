test_that("removeall removes every literal match", {
  x <- c("abcabc", "xyz", NA_character_)

  expect_equal(removeall(x, "ab"), c("cc", "xyz", NA_character_))
})
