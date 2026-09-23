# ---- string helpers --------------------------------------------------------

test_that("basic whitespace/case helpers", {
  expect_equal(trim("  a  "), "a")
  expect_equal(squish("  a   b  "), "a b")
  expect_equal(lower("ABC"), "abc")
  expect_equal(upper("abc"), "ABC")
  expect_equal(titlecase("hello world"), "Hello World")
  expect_equal(sentencecase("HELLO WORLD"), "Hello world")
  expect_equal(textlen(c("abc", NA)), c(3L, NA_integer_))
})

test_that("substring helpers", {
  expect_equal(left("hello", 3), "hel")
  expect_equal(right("hello", 3), "llo")
  expect_equal(middle("hello", 2, 4), "ell")
  expect_equal(truncate("hello world", 8), "hello...")
  expect_equal(truncate("hi", 8), "hi")
  expect_equal(padcenter("hi", 6), "  hi  ")
  expect_equal(padcenter("hi", 6, pad = "*"), "**hi**")
})

test_that("pattern-matching helpers", {
  expect_equal(containstext(c("abc", "xyz"), "b"), c(TRUE, FALSE))
  expect_equal(countmatch("aXaXa", "X"), 2L)
  expect_equal(locate("xxabcxx", "abc")[[1]], 3L)
  expect_equal(unlist(locateall("aXaXa", "X")), c(2L, 4L))
  expect_equal(extract("abc123", "[0-9]+"), "123")
  expect_equal(unlist(extractall("a1b2", "[0-9]")), c("1", "2"))
  expect_equal(extractnum("price: -12.5 usd"), -12.5)
  expect_equal(extractint("value: -42"), -42L)
  expect_equal(extractint("id42x"), 42L)
  expect_equal(extractbetween("[abc]", "\\[", "\\]"), "abc")
})

test_that("replace/remove helpers", {
  expect_equal(replacetext("aXbXc", "X", "-"), "a-b-c")
  expect_equal(removetext("aXbXc", "X"), "abc")
  expect_equal(removeall("aXbXc", "X"), "abc")
  expect_equal(replaceall(c("a", "b", "c"), c("a", "b"), c("A", "B")), c("A", "B", "c"))
})

test_that("split/join helpers", {
  expect_equal(splittext("a-b-c", "-"), list(c("a", "b", "c")))
  expect_equal(splitfirst("a-b-c", "-"), "a")
  expect_equal(splitlast("a-b-c", "-"), "c")
  expect_equal(jointext("a", "b", "c"), "abc")
  expect_equal(collapsetext(c("a", "b", "c"), sep = "-"), "a-b-c")
})

test_that("blank/encoding helpers", {
  expect_equal(isblank(c("", "  ", NA, "x")), c(TRUE, TRUE, TRUE, FALSE))
  expect_equal(removeaccents("café"), "cafe")
  expect_equal(normalizeunicode("abc"), "abc")
  expect_equal(normalizeencoding("abc"), enc2utf8("abc"))
  expect_equal(transliterate("café"), "cafe")
})

test_that("string distance and similarity helpers", {
  d <- textdist("cat", c("cat", "bat", "dog"))
  expect_equal(as.integer(d), c(0L, 1L, 3L))
  expect_equal(similartext("cat", c("bat", "dog")), "bat")
})

test_that("classification predicates", {
  expect_equal(isalpha(c("abc", "a1c", "")), c(TRUE, FALSE, FALSE))
  expect_equal(isalphanumeric(c("abc123", "abc 123")), c(TRUE, FALSE))
  expect_equal(isnumerictext(c("12.5", "-3", "abc")), c(TRUE, TRUE, FALSE))
  expect_equal(isintegertext(c("12", "12.5", "-3")), c(TRUE, FALSE, TRUE))
  expect_equal(isemail(c("a@b.com", "not-an-email")), c(TRUE, FALSE))
  expect_equal(isurl(c("https://x.com", "ftp://x.com", "x.com")), c(TRUE, TRUE, FALSE))
})

test_that("recode/collapse/lump/factor-level helpers", {
  expect_equal(recode(c("a", "b", "c"), "a", "A"), c("A", "b", "c"))
  expect_equal(
    collapsevalues(c("cat", "dog", "bird"), list(pet = c("cat", "dog"))),
    c("pet", "pet", "bird")
  )
  expect_equal(
    collapselevels(factor(c("cat", "dog", "bird")), list(pet = c("cat", "dog"))),
    c("pet", "pet", "bird")
  )

  x <- rep(c("a", "b", "c", "d"), c(10, 5, 3, 1))
  lumped <- lump(x, n = 2, other = "Other")
  expect_equal(sort(unique(lumped)), sort(c("a", "b", "Other")))

  f <- factor(c("low", "high", "mid"), levels = c("low", "mid", "high"))
  expect_equal(levels(reorderlevels(f, c("high", "mid", "low"))), c("high", "mid", "low"))
  expect_equal(levels(expandlevels(f, "extra")), c("low", "mid", "high", "extra"))
})

# ---- string helper regressions ---------------------------------------------

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

test_that("matchestext() with fixed = TRUE does a literal comparison, not a broken anchored regex", {
  expect_true(matchestext("a.b", "a.b", fixed = TRUE))
  expect_false(matchestext("axb", "a.b", fixed = TRUE))
  expect_true(matchestext("axb", "a.b", fixed = FALSE))
})

# ---- transliterate ---------------------------------------------------------

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
