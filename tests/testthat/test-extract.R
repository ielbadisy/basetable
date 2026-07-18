test_that("extract returns capture groups", {
  x <- c("id:123", "id:456", "none")

  expect_equal(extract(x, "id:([0-9]+)"), c("123", "456", NA_character_))
  expect_equal(extract(x, "ID:([0-9]+)", ignore_case = TRUE), c("123", "456", NA_character_))
})
