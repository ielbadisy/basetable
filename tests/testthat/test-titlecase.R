test_that("titlecase converts text to title case", {
  x <- c("hello world", "basetable")

  expect_equal(titlecase(x), c("Hello World", "Basetable"))
})
