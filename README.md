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
| Subset and select | basetable | 1.99 | 311.5 | 6.92 | 1.00 |
| Subset and select | data.table | 1.41 | 709.3 | 3.17 | 0.71 |
| Subset and select | base R | 1.59 | 603.6 | 3.10 | 0.80 |
| Subset and select | dplyr | 2.49 | 367.0 | 4.89 | 1.25 |
| Merge | basetable | 5.66 | 164.0 | 4.82 | 1.00 |
| Merge | data.table | 5.72 | 164.3 | 3.35 | 1.01 |
| Merge | base R | 5.58 | 175.9 | 3.35 | 0.99 |
| Merge | dplyr | 4.58 | 211.2 | 5.77 | 0.81 |
| Aggregate | basetable | 1.75 | 565.9 | 2.85 | 1.00 |
| Aggregate | data.table | 1.98 | 501.7 | 3.12 | 1.13 |
| Aggregate | base R | 30.47 | 32.8 | 28.52 | 17.41 |
| Aggregate | dplyr | 3.32 | 292.2 | 7.00 | 1.90 |

`basetable` wraps `data.table` as its execution backend, so the `data.table`
row is the one that matters most: it isolates wrapper overhead from the
backend's own performance. Across all three workloads basetable tracks
data.table within about 13%, and for aggregation it is effectively at parity
(the large base R gap here is `stats::aggregate`'s formula-interface
overhead, not a basetable result).

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
