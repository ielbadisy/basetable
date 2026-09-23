# Differential checks of the core verbs against base R references, plus
# regressions found by auditing them that way.

plain <- function(x) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  rownames(x) <- NULL
  x
}

audit_frame <- function(n, card = 20L) {
  keys <- sprintf("k%03d", seq_len(card))
  x <- round(stats::rnorm(n), 2)
  x[sample(n, n %/% 20)] <- NA
  if (n > 10) {
    x[sample(n, 2)] <- NaN
    x[sample(n, 2)] <- -0
  }
  data.frame(
    s = sample(c(keys, NA), n, TRUE),
    f = factor(sample(c(letters[1:5], NA), n, TRUE), levels = letters[5:1]),
    i = sample(c(seq_len(card), NA), n, TRUE),
    x = x,
    l = sample(c(TRUE, FALSE, NA), n, TRUE),
    k2 = sample(1:3, n, TRUE),
    stringsAsFactors = FALSE
  )
}

first_appearance_counts <- function(d, by) {
  key <- d[!duplicated(d[by]), by, drop = FALSE]
  id <- match(do.call(paste, c(d[by], sep = "\r")), do.call(paste, c(key, sep = "\r")))
  key$n <- tabulate(id, nrow(key))
  key
}

expect_core_verbs_match_base <- function(d) {
  expect_equal(plain(subset(d, x > 0)), plain(d[which(d$x > 0), ]))
  expect_equal(plain(subset(d, s == "k001" & l)), plain(d[which(d$s == "k001" & d$l), ]))
  expect_equal(plain(subset(d, f == "b", select = c("s", "x"))),
               plain(d[which(d$f == "b"), c("s", "x")]))

  for (by in list("s", "x", "f", c("s", "x"), c("k2", "s", "x"))) {
    for (dec in c(FALSE, TRUE)) {
      want <- d[do.call(order, c(unname(as.list(d[by])),
                                 list(decreasing = dec, method = "radix"))), ]
      expect_equal(plain(orderrows(d, by, decreasing = dec)), plain(want))
    }
  }

  for (cols in list("s", "x", "f", c("s", "i"))) {
    expect_equal(plain(uniquerows(d, cols = cols)),
                 plain(d[!duplicated(d[cols]), cols, drop = FALSE]))
    expect_equal(plain(removeduplicates(d, by = cols, keep = "last")),
                 plain(d[!duplicated(d[cols], fromLast = TRUE), ]))
  }

  for (by in list("s", "i", "f", c("s", "k2"))) {
    expect_equal(plain(count(d, by = by, sort = FALSE)), plain(first_appearance_counts(d, by)))
  }

  key <- d[!duplicated(d["s"]), "s", drop = FALSE]
  g <- factor(match(d$s, key$s), levels = seq_len(nrow(key)))
  for (fn in c("sum", "mean", "sd", "max")) {
    f <- match.fun(fn)
    want <- key
    want$x <- vapply(split(d$x, g), function(v) {
      if (fn == "max" && all(is.na(v))) return(NA_real_)
      as.double(f(v, na.rm = TRUE))
    }, double(1))
    expect_equal(plain(aggregate(d, by = "s", value = "x", fun = f, na.rm = TRUE, sort = FALSE)),
                 plain(want))
  }

  y <- data.frame(s = c("k001", "k003", "k005", "zzz", NA), w = 1:5, stringsAsFactors = FALSE)
  for (all_x in c(FALSE, TRUE)) {
    got <- plain(merge(d, y, by = "s", all.x = all_x))
    want <- plain(base::merge(d, y, by = "s", all.x = all_x, sort = FALSE))[names(got)]
    ord <- function(z) z[do.call(order, c(unname(as.list(z)), list(method = "radix"))), ]
    expect_equal(plain(ord(got)), plain(ord(want)))
  }
  expect_equal(plain(semimerge(d, y, by = "s")), plain(d[d$s %in% y$s, ]))
  expect_equal(plain(antimerge(d, y, by = "s")), plain(d[!d$s %in% y$s, ]))
}

test_that("core verbs match base R on small mixed-type tables", {
  old <- getOption("basetable.threads")
  on.exit(options(basetable.threads = old), add = TRUE)
  set.seed(101)
  for (threads in c(1L, 4L)) {
    setthreads(threads = threads)
    for (n in c(1L, 7L, 300L)) expect_core_verbs_match_base(audit_frame(n))
  }
})

test_that("core verbs match base R on the parallel and string fast paths", {
  skip_on_cran()
  old <- getOption("basetable.threads")
  on.exit(options(basetable.threads = old), add = TRUE)
  set.seed(202)
  for (threads in c(1L, 4L)) {
    setthreads(threads = threads)
    expect_core_verbs_match_base(audit_frame(120000L, card = 2000L))
  }
})

test_that("verbs keep their columns on empty input", {
  d <- audit_frame(0L)
  expect_named(aggregate(d, by = "s", value = "x", fun = function(v) sum(v)), c("s", "x"))
  expect_named(aggregate(d, by = "s", value = "x", fun = sum), c("s", "x"))
  expect_equal(nrow(subset(d, x > 0)), 0L)
  expect_named(count(d, by = "s"), c("s", "n"))
})

test_that("semimerge and antimerge match factor keys by label, not by code", {
  x <- data.frame(f = factor(c("a", "b", "c", "z"), levels = c("z", "c", "b", "a")), v = 1:4)
  y <- data.frame(f = factor(c("a", "c", "q")))
  expect_equal(semimerge(x, y, by = "f")$v, c(1L, 3L))
  expect_equal(antimerge(x, y, by = "f")$v, c(2L, 4L))
  expect_equal(matchedkeys(x, y, by = "f")$v, c(1L, 3L))
})

test_that("merge keeps a factor key a factor, like base::merge()", {
  x <- data.frame(f = factor(c("a", "b"), levels = c("b", "a")), v = 1:2)
  y <- data.frame(f = factor(c("a", "q")), w = 1:2)
  inner <- merge(x, y, by = "f")
  full <- merge(x, y, by = "f", all = TRUE)
  expect_s3_class(inner$f, "factor")
  expect_equal(levels(inner$f), levels(base::merge(x, y, by = "f")$f))
  expect_equal(levels(full$f), levels(base::merge(x, y, by = "f", all = TRUE)$f))
  expect_equal(as.character(full$f), c("a", "b", "q"))
})

test_that("rbindfill keeps factor columns, like base::rbind()", {
  a <- data.frame(s = "u", f = factor("b", levels = c("b", "a")))
  b <- data.frame(s = "v", f = factor("z"))
  expect_equal(rbindfill(a, b)$f, rbind(a, b)$f)
  filled <- rbindfill(a, data.frame(s = "w"))$f
  expect_s3_class(filled, "factor")
  expect_equal(as.character(filled), c("b", NA))
})

test_that("aggregate gives the same result however a native function is passed", {
  d <- data.frame(s = c("a", "a", "b"), x = c(NA, NA, 1))
  f <- min
  expect_identical(aggregate(d, by = "s", value = "x", fun = f, na.rm = TRUE)$x,
                   aggregate(d, by = "s", value = "x", fun = min, na.rm = TRUE)$x)
  expect_identical(aggregate(d, by = "s", value = "x", fun = "min", na.rm = TRUE)$x, c(NA, 1))
  # A user function that shares a native name is called, not replaced.
  local({
    min <- function(v, na.rm = FALSE) -1
    expect_equal(aggregate(d, by = "s", value = "x", fun = min)$x, c(-1, -1))
  })
})

test_that("intersectrows and diffrows without by compare whole rows", {
  x <- data.frame(id = 1:4, g = c("a", "b", "c", "d"))
  y <- x[c(2, 4), ]
  expect_equal(plain(intersectrows(x, y)), plain(x[c(2, 4), ]))
  expect_equal(plain(diffrows(x, y)), plain(x[c(1, 3), ]))
  expect_error(intersectrows(x, y["id"]), "missing column")
})

test_that("towide places single values and only counts duplicated cells", {
  d <- data.frame(id = 1:3, g = c("a", NA, "b"), a = c(1, NA, 3), b = c(4, 5, 6))
  back <- towide(tolong(d, cols = c("a", "b")), names = "variable", values = "value")
  expect_equal(plain(back)[c("id", "g", "a", "b")], plain(d))

  long <- data.frame(id = c(1, 1, 2), k = c("p", "p", "q"), v = c(10, 20, 30))
  expect_message(counted <- towide(long, names = "k", values = "v"), "several values")
  expect_equal(counted$p, c(2L, NA))
  expect_equal(towide(long, names = "k", values = "v", fun = sum)$p, c(30, NA))
})
