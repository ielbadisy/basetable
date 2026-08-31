# Regenerate the benchmark figures shown in README.md.
#
#   Rscript inst/benchmarks/make-readme-figures.R
#
# Writes man/figures/benchmark-time.png and man/figures/benchmark-memory.png
# plus inst/benchmarks/benchmark-results.md (the tables the README embeds).
# Uses bench for both timing and memory, comparing basetable, data.table and
# dplyr. Size is controlled by BT_FIG_N (default 1e6).
#
# Two passes are measured: one on all logical cores and one pinned to a single
# thread. basetable and data.table are always given the same thread budget so
# the two engines are compared on equal footing; dplyr has no parallel path and
# is single-threaded in both passes. The figures use the all-cores pass; the
# single-thread table isolates the algorithmic difference from parallelism.

suppressPackageStartupMessages({
  library(basetable)
  library(bench)
  library(ggplot2)
})

stopifnot(requireNamespace("data.table", quietly = TRUE),
          requireNamespace("dplyr", quietly = TRUE))

N    <- as.integer(Sys.getenv("BT_FIG_N", "1e6"))
REPS <- as.integer(Sys.getenv("BT_FIG_REPS", "15"))

set.seed(1)
n <- N
d <- data.frame(
  g  = sprintf("k%07d", sample(2000L, n, replace = TRUE)),
  gh = sprintf("k%08d", sample(max(n %/% 10L, 1L), n, replace = TRUE)),
  x  = rnorm(n),
  y  = rnorm(n),
  id = seq_len(n),
  stringsAsFactors = FALSE
)
dim_tbl <- d[!duplicated(d$g), c("g", "y")]
dt  <- data.table::as.data.table(d)
dmt <- data.table::as.data.table(dim_tbl)

ops <- c("filter", "sort (string key)", "distinct", "count by group",
         "sd by group", "equi join", "semi join")

bench_one <- function(label, exprs) {
  m <- bench::mark(exprs = exprs, iterations = REPS, check = FALSE, memory = TRUE)
  data.frame(
    operation = label,
    engine    = names(exprs),
    median_ms = as.numeric(m$median) * 1000,
    mem_mb    = as.numeric(m$mem_alloc) / 1024^2,
    stringsAsFactors = FALSE
  )
}

# One full sweep of all operations at whatever thread budget is currently set.
measure_all <- function() {
  res <- do.call(rbind, list(
    bench_one("filter", list(
      basetable  = quote(basetable::subset(d, x > 0.5)),
      data.table = quote(dt[x > 0.5]),
      dplyr      = quote(dplyr::filter(d, x > 0.5)))),
    bench_one("sort (string key)", list(
      basetable  = quote(basetable::orderrows(d, by = c("g", "x"))),
      data.table = quote(data.table::setorder(data.table::copy(dt), g, x)),
      dplyr      = quote(dplyr::arrange(d, g, x)))),
    bench_one("distinct", list(
      basetable  = quote(basetable::uniquerows(d, cols = "g")),
      data.table = quote(unique(dt[, list(g)])),
      dplyr      = quote(dplyr::distinct(d, g)))),
    bench_one("count by group", list(
      basetable  = quote(basetable::count(d, by = "gh", sort = FALSE)),
      data.table = quote(dt[, .N, by = gh]),
      dplyr      = quote(dplyr::count(d, gh)))),
    bench_one("sd by group", list(
      basetable  = quote(basetable::aggregate(d, by = "g", value = "x", fun = sd, sort = FALSE)),
      data.table = quote(dt[, list(x = sd(x)), by = g]),
      dplyr      = quote(dplyr::summarise(dplyr::group_by(d, g), x = sd(x), .groups = "drop")))),
    bench_one("equi join", list(
      # basetable::merge() returns rows in input order; pin data.table to
      # sort = FALSE so both materialise the join without also sorting it.
      basetable  = quote(basetable::merge(d, dim_tbl, by = "g")),
      data.table = quote(merge(dt, dmt, by = "g", sort = FALSE)),
      dplyr      = quote(dplyr::inner_join(d, dim_tbl, by = "g")))),
    bench_one("semi join", list(
      basetable  = quote(basetable::semimerge(d, dim_tbl, by = "g")),
      data.table = quote(dt[dmt, on = "g", nomatch = NULL]),
      dplyr      = quote(dplyr::semi_join(d, dim_tbl, by = "g"))))
  ))
  res$operation <- factor(res$operation, levels = ops)
  res$engine    <- factor(res$engine, levels = c("basetable", "data.table", "dplyr"))
  res
}

# Give basetable and data.table the same budget on every pass.
set_threads <- function(threads = NULL, percent = NULL) {
  if (!is.null(percent)) {
    basetable::setthreads(percent = percent)
    data.table::setDTthreads(percent = percent)
  } else {
    basetable::setthreads(threads = threads)
    data.table::setDTthreads(threads = threads)
  }
  getOption("basetable.threads", 1L)
}

nthreads_mt <- set_threads(percent = 100)
res_mt <- measure_all()

set_threads(threads = 1L)
res_st <- measure_all()

# Restore a full budget for anything downstream (figures, interactive use).
set_threads(percent = 100)

pal <- c(basetable = "#1b7837", data.table = "#762a83", dplyr = "#c2a5cf")
base_theme <- theme_minimal(base_size = 12) +
  theme(legend.position = "none", strip.text = element_text(face = "bold"),
        panel.grid.minor = element_blank())

dir.create("man/figures", showWarnings = FALSE, recursive = TRUE)

p_time <- ggplot(res_mt, aes(engine, median_ms, fill = engine)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = round(median_ms)), hjust = -0.15, size = 3.2) +
  facet_wrap(~operation, ncol = 2, scales = "free_x") +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  scale_fill_manual(values = pal) +
  labs(x = NULL, y = "Median time (ms)", fill = NULL,
       title = sprintf("Speed: %s rows, %d threads (dplyr is single-threaded)",
                       format(N, big.mark = ","), nthreads_mt)) +
  base_theme

p_mem <- ggplot(res_mt, aes(engine, mem_mb, fill = engine)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = ifelse(mem_mb < 1, sprintf("%.2f", mem_mb),
                               sprintf("%.0f", mem_mb))), hjust = -0.15, size = 3.2) +
  facet_wrap(~operation, ncol = 2, scales = "free_x") +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.20))) +
  scale_fill_manual(values = pal) +
  labs(x = NULL, y = "Memory allocated (MB)", fill = NULL,
       title = sprintf("Memory: %s rows", format(N, big.mark = ","))) +
  base_theme

ggsave("man/figures/benchmark-time.png", p_time, width = 9, height = 7, dpi = 130)
ggsave("man/figures/benchmark-memory.png", p_mem, width = 9, height = 7, dpi = 130)

emit_table <- function(res, header) {
  wide <- reshape(res, idvar = "operation", timevar = "engine", direction = "wide")
  names(wide) <- sub("median_ms\\.", "t_", names(wide))
  names(wide) <- sub("mem_mb\\.", "m_", names(wide))
  out <- c(
    header,
    "",
    "| Operation | basetable | data.table | dplyr | basetable mem | data.table mem | dplyr mem |",
    "| --- | ---: | ---: | ---: | ---: | ---: | ---: |"
  )
  for (i in seq_len(nrow(wide))) {
    w <- wide[i, ]
    out <- c(out, sprintf(
      "| %s | %.0f ms | %.0f ms | %.0f ms | %.2f MB | %.2f MB | %.2f MB |",
      w$operation, w$t_basetable, w$t_data.table, w$t_dplyr,
      w$m_basetable, w$m_data.table, w$m_dplyr))
  }
  out
}

lines <- c(
  sprintf("<!-- generated by inst/benchmarks/make-readme-figures.R; %s rows -->",
          format(N, big.mark = ",")),
  "",
  emit_table(res_mt, sprintf(
    "### All cores (%d threads; dplyr single-threaded)", nthreads_mt)),
  "",
  emit_table(res_st, "### Single thread (basetable and data.table pinned to 1)")
)
writeLines(lines, "inst/benchmarks/benchmark-results.md")
cat("wrote figures + inst/benchmarks/benchmark-results.md\n")
cat("\n== all cores (", nthreads_mt, "threads) ==\n")
print(res_mt)
cat("\n== single thread ==\n")
print(res_st)
