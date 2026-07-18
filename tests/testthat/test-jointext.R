test_that("jointext joins text with a separator", {
  expect_equal(jointext("a", "b", "c", sep = "-"), "a-b-c")
})
