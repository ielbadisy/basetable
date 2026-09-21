# Benchmark basetable::nest() / unnest() against tidyr and dplyr.
#
#   Rscript inst/benchmarks/benchmark-nest.R
#
# Scenarios vary the number of groups at a fixed 1e6 rows. Results are
# checked for equal content before timing.

for (pkg in c("basetable", "tidyr", "dplyr", "bench")) {
  requireNamespace(pkg, quietly = TRUE) || stop("needs ", pkg)
}

set.seed(1)
n <- 1e6L
scenarios <- c("10 groups" = 1e1, "1e3 groups" = 1e3, "1e5 groups" = 1e5)

run <- function(n_groups) {
  d <- data.frame(
    g = sample.int(n_groups, n, TRUE),
    x = rnorm(n),
    y = sample.int(100L, n, TRUE),
    z = sample(letters, n, TRUE)
  )
  tb <- dplyr::as_tibble(d)

  bt_n <- basetable::nest(d, by = "g")
  td_n <- tidyr::nest(tb, data = c(x, y, z))
  dp_n <- dplyr::group_nest(tb, g)

  norm <- function(u) {
    u <- as.data.frame(u)[c("g", "x", "y", "z")]
    u[do.call(order, u), ]
  }
  ref <- norm(tidyr::unnest(td_n, data))
  stopifnot(
    isTRUE(all.equal(norm(basetable::unnest(bt_n, "data")), ref, check.attributes = FALSE))
  )

  nest_res <- bench::mark(
    basetable = basetable::nest(d, by = "g"),
    tidyr = tidyr::nest(tb, data = c(x, y, z)),
    dplyr = dplyr::group_nest(tb, g),
    check = FALSE, min_iterations = 3, memory = TRUE
  )
  unnest_res <- bench::mark(
    basetable = basetable::unnest(bt_n, "data"),
    tidyr = tidyr::unnest(td_n, data),
    dplyr = tidyr::unnest(dp_n, data),
    check = FALSE, min_iterations = 3, memory = TRUE
  )
  list(nest = nest_res, unnest = unnest_res)
}

fmt <- function(res, op, label) {
  data.frame(
    scenario = label, operation = op,
    engine = as.character(res$expression),
    median = as.character(res$median),
    mem = as.character(res$mem_alloc)
  )
}

out <- do.call(rbind, Map(function(ng, label) {
  r <- run(ng)
  rbind(fmt(r$nest, "nest", label), fmt(r$unnest, "unnest", label))
}, scenarios, names(scenarios)))
print(out, row.names = FALSE)
