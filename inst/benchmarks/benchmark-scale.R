# basetable scale benchmark
# ---------------------------------------------------------------------------
# Runtime + allocated memory for the core verbs across data size and key
# cardinality, versus base R / data.table / dplyr. Every
# competitor is optional: an engine whose package is not installed is skipped.
#
# Config via environment variables (all comma-separated):
#   BT_SIZES   row counts              default "1e6,1e7"   (add 1e8 if you have the RAM)
#   BT_CARD    distinct group keys     default "10,1000,100000"
#   BT_REPS    repetitions per cell    default "5"
#   BT_ENGINES limit to these engines  default all installed
#   BT_OUT     results CSV path        default "bt-bench-results.csv"
#
# Run from the package root:
#   Rscript inst/benchmarks/benchmark-scale.R
# ---------------------------------------------------------------------------

suppressPackageStartupMessages(library(basetable))

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
env_nums <- function(key, default) {
  v <- Sys.getenv(key, default)
  as.numeric(strsplit(v, ",", fixed = TRUE)[[1L]])
}

SIZES <- as.numeric(env_nums("BT_SIZES", "1e6,1e7"))
CARDS <- as.integer(env_nums("BT_CARD", "10,1000,100000"))
REPS  <- as.integer(env_nums("BT_REPS", "5"))[1L]
OUT   <- Sys.getenv("BT_OUT", "bt-bench-results.csv")

has <- function(p) requireNamespace(p, quietly = TRUE)
ENGINES <- c("basetable", "base", "data.table", "dplyr")
ENGINES <- ENGINES[ENGINES %in% c("basetable", "base") | vapply(ENGINES, has, logical(1))]
want <- Sys.getenv("BT_ENGINES", "")
if (nzchar(want)) ENGINES <- intersect(ENGINES, strsplit(want, ",", fixed = TRUE)[[1L]])
message("engines: ", paste(ENGINES, collapse = ", "))

# --- timing -----------------------------------------------------------------
time_cell <- function(fun) {
  fun()  # warm up / surface errors
  gc(FALSE)
  m0 <- sum(gc(FALSE)[, "used"] * c(NA, 8)[2], na.rm = TRUE)
  base_mem <- sum(gc(FALSE)[, 2] * c(56, 8)[2])
  t <- replicate(REPS, {
    g0 <- gc(FALSE)
    tt <- system.time(res <<- fun())[["elapsed"]]
    g1 <- gc(FALSE)
    attr(tt, "mem") <- sum((g1[, 1] - g0[, 1]) * c(56, 0)) + (g1[2, 1] - g0[2, 1]) * 8 * 1024
    tt
  }, simplify = FALSE)
  ms <- median(vapply(t, as.numeric, numeric(1))) * 1000
  mb <- median(vapply(t, function(z) attr(z, "mem"), numeric(1))) / 1024^2
  list(ms = ms, mb = max(mb, 0), value = res)
}

safe <- function(engine, fun) {
  if (!engine %in% ENGINES) return(NULL)
  tryCatch(time_cell(fun), error = function(e) {
    message(sprintf("  ! %s: %s", engine, conditionMessage(e)))
    list(ms = NA_real_, mb = NA_real_, value = NULL)
  })
}

# --- data -----------------------------------------------------------------
make_frame <- function(n, k, seed = 1L) {
  set.seed(seed)
  data.frame(
    id    = seq_len(n),
    g     = sprintf("k%08d", sample.int(k, n, replace = TRUE)),
    gi    = sample.int(k, n, replace = TRUE),
    t     = sort(runif(n) * n),
    x     = rnorm(n),
    w     = runif(n),
    stringsAsFactors = FALSE
  )
}

rows <- list()
add <- function(...) rows[[length(rows) + 1L]] <<- data.frame(..., stringsAsFactors = FALSE)

for (n in SIZES) {
  for (k in CARDS) {
    if (k > n) next
    message(sprintf("n=%.0e  cardinality=%d", n, k))
    df <- make_frame(as.integer(n), k)
    dim_tbl <- df[!duplicated(df$gi), c("gi", "w")][seq_len(min(k, 1000L)), ]
    # Rolling join inputs are pre-sorted once, outside every timer, so no arm
    # pays for an ordering step the others skip.
    df_sorted <- df[order(df$g, df$t), ]
    y_roll <- df[sample(nrow(df), min(nrow(df), 100000L)), c("g", "t", "x")]
    y_roll <- y_roll[order(y_roll$g, y_roll$t), ]

    engines_data <- list(base = df, basetable = df)
    if ("data.table" %in% ENGINES) engines_data$data.table <- data.table::as.data.table(df)
    if ("dplyr" %in% ENGINES)      engines_data$dplyr <- df

    ops <- list(
      filter = list(
        basetable  = function() basetable::subset(df, x > 0.5),
        base       = function() df[df$x > 0.5, , drop = FALSE],
        data.table = function() engines_data$data.table[x > 0.5],
        dplyr      = function() dplyr::filter(df, x > 0.5)
      ),
      sort_str = list(
        basetable  = function() basetable::orderrows(df, by = c("g", "x")),
        base       = function() df[order(df$g, df$x), , drop = FALSE],
        data.table = function() data.table::setorder(data.table::copy(engines_data$data.table), g, x),
        dplyr      = function() dplyr::arrange(df, g, x)
      ),
      distinct = list(
        basetable  = function() basetable::uniquerows(df, cols = "g"),
        base       = function() unique(df[, "g", drop = FALSE]),
        data.table = function() unique(engines_data$data.table[, list(g)]),
        dplyr      = function() dplyr::distinct(df, g)
      ),
      count_by = list(
        basetable  = function() basetable::count(df, by = "g", sort = FALSE),
        base       = function() as.data.frame(table(df$g)),
        data.table = function() engines_data$data.table[, .N, by = g],
        dplyr      = function() dplyr::count(df, g)
      ),
      sum_by = list(
        basetable  = function() basetable::aggregate(df, by = "g", value = "x", fun = sum, sort = FALSE),
        base       = function() rowsum(df$x, df$g),
        data.table = function() engines_data$data.table[, list(x = sum(x)), by = g],
        dplyr      = function() dplyr::summarise(dplyr::group_by(df, g), x = sum(x), .groups = "drop")
      ),
      sd_by = list(
        basetable  = function() basetable::aggregate(df, by = "g", value = "x", fun = sd, sort = FALSE),
        base       = function() tapply(df$x, df$g, sd),
        data.table = function() engines_data$data.table[, list(x = sd(x)), by = g],
        dplyr      = function() dplyr::summarise(dplyr::group_by(df, g), x = sd(x), .groups = "drop")
      ),
      join_id = list(
        basetable  = function() basetable::merge(df, dim_tbl, by = "gi"),
        base       = function() merge(df, dim_tbl, by = "gi"),
        data.table = function() merge(engines_data$data.table, data.table::as.data.table(dim_tbl), by = "gi"),
        dplyr      = function() dplyr::inner_join(df, dim_tbl, by = "gi")
      ),
      roll_join = list(
        basetable  = function() basetable::rollingmerge(df_sorted, y_roll, by = c("g", "t"), direction = "backward"),
        data.table = function() {
          a <- data.table::as.data.table(df_sorted); b <- data.table::as.data.table(y_roll)
          data.table::setattr(a, "sorted", c("g", "t")); data.table::setattr(b, "sorted", c("g", "t"))
          b[a, roll = TRUE, on = c("g", "t")]
        }
      )
    )

    for (op in names(ops)) {
      arms <- ops[[op]]
      for (eng in names(arms)) {
        r <- safe(eng, arms[[eng]])
        if (is.null(r)) next
        add(rows = n, cardinality = k, operation = op, engine = eng,
            median_ms = round(r$ms, 3), memory_mb = round(r$mb, 2))
      }
    }
  }
}

res <- do.call(rbind, rows)
write.csv(res, OUT, row.names = FALSE)
message("wrote ", OUT, " (", nrow(res), " rows)")

# --- summary: ratio to data.table where present ---------------------------
if (all(c("engine", "median_ms") %in% names(res))) {
  w <- reshape(res[c("rows", "cardinality", "operation", "engine", "median_ms")],
               idvar = c("rows", "cardinality", "operation"),
               timevar = "engine", direction = "wide")
  names(w) <- sub("^median_ms\\.", "", names(w))
  if ("data.table" %in% names(w) && "basetable" %in% names(w)) {
    w$bt_vs_dt <- round(w$basetable / w$`data.table`, 2)
  }
  print(w, row.names = FALSE)
}
