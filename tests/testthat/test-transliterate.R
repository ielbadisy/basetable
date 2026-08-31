test_that("removeaccents folds Latin-1 and Latin Extended-A to ICU Latin-ASCII", {
  expect_equal(
    removeaccents(c("café", "naïve", "Zürich", "Kraków",
                    "Æther", "œuvre", "Straße", "Þór",
                    "Đorđe", "piñata", "hôtel", "Málaga",
                    "Łódź", "Ångström", "Nîmes",
                    "Timișoara", "İstanbul")),
    c("cafe", "naive", "Zurich", "Krakow", "AEther", "oeuvre", "Strasse",
      "THor", "Dorde", "pinata", "hotel", "Malaga", "Lodz", "Angstrom",
      "Nimes", "Timisoara", "Istanbul")
  )
})

test_that("removeaccents leaves ASCII and unmapped scripts untouched", {
  expect_equal(removeaccents(c("plain ascii 123", "a-b_c.d")),
               c("plain ascii 123", "a-b_c.d"))
  # Greek is not folded by removeaccents(); it only strips Latin accents.
  expect_equal(removeaccents("Αθήνα"),
               "Αθήνα")
})

test_that("removeaccents handles NA, empty, factor and non-character input", {
  expect_identical(removeaccents(NA_character_), NA_character_)
  expect_identical(removeaccents(""), "")
  expect_identical(removeaccents(c("café", NA)), c("cafe", NA))
  expect_identical(removeaccents(factor("café")), "cafe")
  expect_identical(removeaccents(1:3), c("1", "2", "3"))
})

test_that("transliterate romanises Greek and Cyrillic like ICU Any-Latin", {
  expect_equal(
    transliterate(c("Αθήνα",       # Athena
                    "Ελλάδα",  # Ellada
                    "Μοσχα",        # Moscha
                    "Москва",  # Moskva
                    "Достоевский", # Dostoevskij
                    "Чайковский")),      # Cajkovskij
    c("Athena", "Ellada", "Moscha", "Moskva", "Dostoevskij", "Cajkovskij")
  )
})

test_that("transliterate also does everything removeaccents does", {
  expect_equal(transliterate(c("café", "Zürich", "Straße")),
               c("cafe", "Zurich", "Strasse"))
})

test_that("transliterate leaves scripts it does not cover unchanged", {
  han <- "北京"
  expect_equal(transliterate(han), han)
})
