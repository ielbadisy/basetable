test_that("textdist computes edit distances", {
  out <- textdist(c("cat", "bat"))

  expect_equal(out[1, 2], 1)
  expect_equal(out[2, 1], 1)
})
