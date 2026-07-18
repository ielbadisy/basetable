test_that("splittext splits text on a literal delimiter", {
  x <- c("a,b,c", "x,y", NA_character_)

  out <- splittext(x, ",")

  expect_type(out, "list")
  expect_equal(out[[1]], c("a", "b", "c"))
  expect_equal(out[[2]], c("x", "y"))
  expect_true(is.na(out[[3]]))
})
