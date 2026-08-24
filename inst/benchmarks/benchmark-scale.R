library(basetable)
library(bench)
library(data.table)

# Reproducible scale benchmark for runtime and allocated memory. Override the
# defaults with comma-separated values in BASETABLE_BENCH_SIZES when a smaller
# development pass is useful.
parsesizes <- function() {
  configured <- Sys.getenv("BASETABLE_BENCH_SIZES", "1e5,1e6,1e7")
  sizes <- as.numeric(strsplit(configured, ",", fixed = TRUE)[[1L]])
  if (length(sizes) == 0L || any(!is.finite(sizes)) || any(sizes < 1)) {
    stop("BASETABLE_BENCH_SIZES must contain positive comma-separated numbers.")
  }
  as.integer(sizes)
}

iterationsfor <- function(size) {
  if (size <= 1e5) 15L else if (size <= 1e6) 7L else 3L
}

makedata <- function(size, seed = 1L) {
  set.seed(seed)
  data.table(
    id = seq_len(size),
    group = sample(letters[1:20], size, replace = TRUE),
    score = sample.int(1000L, size, replace = TRUE),
    value = rnorm(size)
  )
}

summarisebench <- function(result, size, operation) {
  data.table::as.data.table(result)[, .(
    rows = size,
    operation = operation,
    implementation = as.character(expression),
    median_ms = as.numeric(median) * 1000,
    iterations_per_sec = as.numeric(`itr/sec`),
    memory_mb = as.numeric(mem_alloc) / 1024^2,
    garbage_collections = n_gc,
    iterations = n_itr
  )]
}

markscale <- function(data, size, iterations) {
  filterresult <- bench::mark(
    basetable = basetable::filter(data, value > 0),
    data_table = data[value > 0],
    iterations = iterations,
    check = FALSE,
    memory = TRUE
  )

  transformresult <- bench::mark(
    basetable = basetable::transform(data, adjusted = value * 2),
    data_table = {
      out <- data.table::copy(data)
      out[, adjusted := value * 2]
    },
    iterations = iterations,
    check = FALSE,
    memory = TRUE
  )

  countresult <- bench::mark(
    basetable = basetable::count(data, by = "group"),
    data_table = data[, .N, by = group],
    iterations = iterations,
    check = FALSE,
    memory = TRUE
  )

  orderresult <- bench::mark(
    basetable = basetable::orderrows(
      data,
      by = c("group", "score"),
      decreasing = c(FALSE, TRUE)
    ),
    data_table = {
      out <- data.table::copy(data)
      data.table::setorderv(out, c("group", "score"), c(1L, -1L))
    },
    iterations = iterations,
    check = FALSE,
    memory = TRUE
  )

  rbindlist(list(
    summarisebench(filterresult, size, "filter"),
    summarisebench(transformresult, size, "transform"),
    summarisebench(countresult, size, "count"),
    summarisebench(orderresult, size, "orderrows")
  ))
}

sizes <- parsesizes()
results <- vector("list", length(sizes))

for (i in seq_along(sizes)) {
  size <- sizes[[i]]
  message("Benchmarking ", format(size, big.mark = ","), " rows...")
  data <- makedata(size)
  results[[i]] <- markscale(data, size, iterationsfor(size))
  rm(data)
  gc()
}

results <- rbindlist(results)
results[, `:=`(
  time_overhead = median_ms / median_ms[implementation == "data_table"],
  memory_overhead = memory_mb / memory_mb[implementation == "data_table"]
), by = .(rows, operation)]
setorder(results, rows, operation, implementation)

output <- Sys.getenv("BASETABLE_BENCH_OUTPUT")
if (nzchar(output)) {
  data.table::fwrite(results, output)
  message("Wrote ", output)
} else {
  print(results)
}
