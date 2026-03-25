library(basetable)
library(data.table)

set.seed(1)
n <- 5e4
x <- data.frame(id = seq_len(n), value_x = rnorm(n))
y <- data.frame(id = sample(seq_len(n), n), value_y = rnorm(n))

bench_merge <- function() {
  system.time(merge(x, y, by = "id"))
  system.time(data.table::merge.data.table(as.data.table(x), as.data.table(y), by = "id"))
}

print(bench_merge())
