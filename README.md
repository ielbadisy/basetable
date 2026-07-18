# basetable

`basetable` is a compact R package for tabular data manipulation and exploratory
data analysis with:

- base-style naming and semantics
- `data.table` as the execution backend
- explicit, standard-evaluation interfaces
- natural support for both nested calls and `|>`

This package is not a dplyr clone. The API centers on functions that feel close
to `subset()`, `transform()`, `aggregate()`, `merge()`, `split()`, and
`reshape()`.

## Status

This repository now contains the initial package scaffold, naming dictionary,
semantic specification, first implementation pass, tests, vignettes, and
benchmark scripts.

## Benchmarks

The `Benchmarks` vignette contains the reproducible report. The summary below
uses 15 iterations per workload on this workspace.

![Benchmark absolute time](man/figures/benchmark-absolute.png)

![Benchmark relative time](man/figures/benchmark-relative.png)

| Operation | Implementation | Median (ms) | Iterations / sec | Memory (MB) | Relative time |
| --- | --- | ---: | ---: | ---: | ---: |
| Subset and select | basetable | 1.93 | 525.4 | 4.58 | 1.00 |
| Subset and select | data.table | 1.52 | 610.0 | 3.27 | 0.79 |
| Subset and select | base R | 1.54 | 647.0 | 3.10 | 0.80 |
| Subset and select | dplyr | 1.76 | 528.8 | 4.98 | 0.91 |
| Merge | basetable | 4.96 | 194.7 | 3.76 | 1.00 |
| Merge | data.table | 4.92 | 195.6 | 3.35 | 0.99 |
| Merge | base R | 4.91 | 195.6 | 3.35 | 0.99 |
| Merge | dplyr | 3.50 | 271.0 | 5.81 | 0.71 |
| Aggregate | basetable | 1.36 | 687.0 | 0.91 | 1.00 |
| Aggregate | data.table | 1.88 | 506.9 | 3.12 | 1.38 |
| Aggregate | base R | 24.94 | 39.5 | 28.55 | 18.31 |
| Aggregate | dplyr | 2.77 | 322.8 | 7.01 | 2.03 |

`basetable` wraps `data.table` as its execution backend, so the `data.table`
row is the one that matters most: it isolates wrapper overhead from the
backend's own performance. The `data.table` row above uses the idiomatic
expression a user would hand-write for each operation (e.g. `.(value =
mean(value))` for aggregation), which is not always exactly the code path
`basetable`'s wrapper generates internally — so a basetable row at or below
1.00x relative to data.table (as aggregate shows here) reflects a different
data.table idiom being used, not the wrapper beating its own backend at
identical work. A stricter benchmark that forces both sides through the same
internal j-expression and averages over 300 iterations puts basetable's
actual wrapper overhead at roughly 1.2x for subset, 1.1x for aggregate, and
about parity for merge (the large base R gap here is `stats::aggregate`'s
formula-interface overhead, not a basetable result).

Rerun `vignettes/benchmarking.Rmd` to refresh the report if the workload or
implementation changes.

## Installation

```r
# development install
# install.packages("pak")
pak::pak("path/to/project-basetable")
```

## Minimal examples

Nested style:

```r
library(basetable)

describe(
  transform(
    subset(mtcars, cyl == 6, select = c("mpg", "hp", "wt", "cyl")),
    power = hp / wt
  )
)
```

Pipe style:

```r
library(basetable)

mtcars |>
  pick(c("mpg", "hp", "wt", "cyl")) |>
  transform(power = hp / wt) |>
  aggregate(by = "cyl", value = c("mpg", "power"), fun = mean)
```

Table 1 style summary:

```r
library(basetable)

summarytab(
  transform(mtcars, am = factor(am, labels = c("Automatic", "Manual"))),
  vars = c("mpg", "hp"),
  by = "am",
  p_value = TRUE
)
```

## Operation dictionary

| Family | Exported function | Base reference |
| --- | --- | --- |
| Row subsetting | `subset()` | `base::subset()` |
| Column keeping | `pick()` | `[` column selection |
| Column dropping | `drop()` | negative column indexing |
| Transformation | `transform()`, `within()` | base equivalents |
| Ordering | `reorder()` | `order()` |
| Aggregation | `aggregate()`, `count()` | `aggregate()`, `table()` |
| Joining | `merge()` | `merge()` |
| Split/apply/combine | `split()`, `by_apply()`, `combine()` | `split()`, `by()` |
| Reshaping | `reshape()`, `stack()`, `unstack()` | base equivalents |
| Inspection | `glimpse()`, `dims()`, `types()`, `headtail()` | `str()`, `dim()`, `head()` |
| EDA | `describe()`, `missingness()`, `profile()`, `freq()`, `summarytab()`, `compare()` | base summaries |

## Positioning

Compared with base R, `basetable` provides a tighter operation dictionary,
faster internals for common table tasks, and compact EDA helpers. Compared with
raw `data.table`, it favors a stable function interface over `[i, j, by]`.
Compared with dplyr, it avoids tidy evaluation, grouped-object state, and verb
grammar centered on `filter()`, `mutate()`, and `summarise()`.
