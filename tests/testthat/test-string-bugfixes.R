test_that("padleft/padright honor a custom pad character", {
  expect_equal(padleft("5", 3, pad = "0"), "005")
  expect_equal(padright("5", 3, pad = "0"), "500")
  expect_equal(padleft(c("1", "22", "333"), 4, pad = "*"), c("***1", "**22", "*333"))
})

test_that("startswith/endswith honor fixed and stay vectorized against x", {
  # fixed = TRUE: "a." is a literal prefix, not a regex
  expect_true(startswith("a.b", "a.", fixed = TRUE))
  expect_false(startswith("axb", "a.", fixed = TRUE))

  # fixed = FALSE (default): real regex support
  expect_equal(startswith(c("abc", "xbc"), "a.c"), c(TRUE, FALSE))
  expect_equal(endswith(c("abc", "abd"), ".c"), c(TRUE, FALSE))

  # endswith must return one value per element of x, not a matrix
  out <- endswith(c("abc", "xyz", "abd"), "c")
  expect_null(dim(out))
  expect_equal(out, c(TRUE, FALSE, FALSE))
})

test_that("matches() with fixed = TRUE does a literal comparison, not a broken anchored regex", {
  expect_true(matches("a.b", "a.b", fixed = TRUE))
  expect_false(matches("axb", "a.b", fixed = TRUE))
  expect_true(matches("axb", "a.b", fixed = FALSE))
})
