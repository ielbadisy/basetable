library(basetable)
library(data.table)

set.seed(1)

`%||%` <- function(x, y) if (is.null(x)) y else x

sizes <- as.integer(strsplit(Sys.getenv("BT_BENCH_SIZES", "100000,1000000"), ",", fixed = TRUE)[[1]])
iterations <- as.integer(Sys.getenv("BT_BENCH_ITERATIONS", "5"))

time_one <- function(expr, env) {
  gc()
  stats::median(replicate(iterations, system.time(eval(expr, envir = env))[["elapsed"]]))
}

record <- function(size, operation, engine, expr) {
  expr <- substitute(expr)
  env <- parent.frame()
  elapsed <- tryCatch(time_one(expr, env), error = function(e) structure(NA_real_, error = conditionMessage(e)))
  data.table(
    size = size,
    operation = operation,
    engine = engine,
    seconds = as.numeric(elapsed),
    error = attr(elapsed, "error") %||% NA_character_
  )
}

run_size <- function(n) {
  dt <- data.table(
    id = seq_len(n),
    g_int = sample.int(1000L, n, replace = TRUE),
    g_chr = sample(sprintf("g%04d", 1:1000), n, replace = TRUE),
    x = runif(n),
    y = rnorm(n)
  )
  df <- as.data.frame(dt)

  out <- list(
    record(n, "project", "basetable", basetable::pick(df, c("g_int", "x"))),
    record(n, "project", "data.table", dt[, .(g_int, x)]),
    record(n, "filter_project", "basetable", basetable::subset(df, g_int <= 10L, select = c(g_int, x))),
    record(n, "filter_project", "data.table", dt[g_int <= 10L, .(g_int, x)]),
    record(n, "count_int", "basetable", basetable::count(df, by = "g_int", sort = FALSE)),
    record(n, "count_int", "data.table", dt[, .N, by = g_int]),
    record(n, "unique_int", "basetable", basetable::uniquerows(df, cols = "g_int")),
    record(n, "unique_int", "data.table", unique(dt[, .(g_int)])),
    record(n, "mean_by_int", "basetable", basetable::aggregate(df, by = "g_int", value = "x", fun = mean, sort = FALSE)),
    record(n, "mean_by_int", "data.table", dt[, .(x = mean(x)), by = g_int]),
    record(n, "semijoin_int", "basetable", basetable::semimerge(df, df[1:1000, "g_int", drop = FALSE], by = "g_int")),
    record(n, "semijoin_int", "data.table", dt[unique(dt[1:1000, .(g_int)]), on = "g_int", nomatch = NULL])
  )

  data.table::rbindlist(out, fill = TRUE)
}

results <- data.table::rbindlist(lapply(sizes, run_size), fill = TRUE)
data.table::setorder(results, size, operation, seconds)
print(results)

wide <- data.table::dcast(results[is.na(error)], size + operation ~ engine, value.var = "seconds")
if ("basetable" %in% names(wide)) {
  for (engine in setdiff(names(wide), c("size", "operation", "basetable"))) {
    wide[, paste0("basetable_vs_", engine) := basetable / get(engine)]
  }
}
print(wide)
