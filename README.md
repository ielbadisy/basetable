# basetable

<!-- badges: start -->
[![R-CMD-check](https://github.com/ielbadisy/basetable/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ielbadisy/basetable/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

`basetable` is a fast in-memory data-manipulation package for R with a
**base-R interface** and **no dependencies**. You write `subset()`,
`transform()`, `aggregate()`, `merge()`, `split()`, and the work runs on the
package's own C++ engine. There is no `data.table`, no `dplyr`, no Arrow
underneath — just `parallel`, `stats`, `stringi`, `utils`.

Every verb returns a `basetable`: an ordinary `data.frame` with one extra
class so it prints compactly and `[` keeps the class. `as.data.frame()`
strips it back to a plain frame.

This is a deliberately focused tool. It is aimed at

- people **teaching or learning base R** who want speed without the cognitive
  load of tidy evaluation or `[i, j, by]`, and
- codebases already built on `subset()` / `merge()` / `aggregate()` that want
  a faster engine **without a rewrite**.

## Design

- **Base-style naming and semantics.** Functions read like `subset()`,
  `transform()`, `aggregate()`, `merge()`, `split()`.
- **A native C++ engine.** Projection, filtering, ordering, distinct,
  grouping, all join kinds, row-bind and `subset()` predicate evaluation run
  in compiled `.Call` kernels. There is no third-party compute backend.
- **Explicit, standard-evaluation interfaces.** Column names are strings, not
  captured symbols (with the marked exceptions `subset()` and `transform()`
  inherit from base R).
- **Zero hard dependencies.** `data.table`, `dplyr`, `collapse` and `polars`
  appear only in `Suggests`, and only as competitors in the benchmark
  vignette.

## Installation

```r
# install.packages("pak")
pak::pak("ielbadisy/basetable")
```

## Minimal examples

```r
library(basetable)

# nested
describe(
  transform(
    subset(mtcars, cyl == 6, select = c("mpg", "hp", "wt", "cyl")),
    power = hp / wt
  )
)

# pipe
mtcars |>
  pick(c("mpg", "hp", "wt", "cyl")) |>
  transform(power = hp / wt) |>
  aggregate(by = "cyl", value = c("mpg", "power"), fun = mean)

# table-1 style summary
summarytab(
  transform(mtcars, am = factor(am, labels = c("Automatic", "Manual"))),
  vars = c("mpg", "hp"), by = "am", p_value = TRUE
)
```

## Performance

The engine holds its own against `data.table` without depending on it, and
allocates far less on grouped operations. At 1e6 rows (median of 5, one Linux
machine; `inst/benchmarks/benchmark-scale.R`), time relative to `data.table`:

| Operation | few groups | many groups |
| --- | ---: | ---: |
| `uniquerows()` | **0.55x** | **0.58x** |
| `aggregate(fun = sd)` | **0.87x** | **0.67x** |
| grouped `count()` | 1.5x | **0.62x** |
| `merge()` (equi) | **0.84x** | 1.0x |
| `rollingmerge()` | **0.62x** | 1.9x |
| `aggregate(fun = sum)` | 2.6x | 1.06x |
| `subset()` | 2.1x | 1.9x |
| `orderrows()` (string key) | 8.9x | 4.9x |

Grouped reducers accumulate in C++ without materialising intermediate
columns, so a grouped `aggregate` or `count` allocates ~0.01 MB where the
other engines allocate 3–28 MB. Grouped `aggregate` also reduces in parallel
above ~750k rows.

The open gap is **string sorting** — `orderrows()` pre-ranks character
columns to integers but still runs a comparison sort where `data.table` uses
a radix sort. `collapse` and `polars` are also 2–20x faster on columnar and
grouped paths; matching their parallel-radix / Arrow engines is not a near
goal.

The `Benchmarks` vignette has the full reproducible report against base R,
`data.table`, `dplyr`, `collapse` and `polars`.

## How basetable compares

`data.table`, `collapse` and `polars` are faster on many grouped and columnar
workloads and have far larger ecosystems; `dplyr` is the tidyverse standard.
`basetable` is a good fit when you want:

- **base-R syntax** and semantics, not `[i, j, by]`, tidy evaluation, or a
  method-chained frame object;
- **no dependencies** to install, pin, or reason about;
- a package small enough to **read end to end**, teach from, and hand to a
  language model as a stable target;
- competitive speed and best-in-class memory on the everyday operations
  (filter, group, join, distinct) without changing how you write code.

## Using basetable alongside dplyr and data.table

`basetable` reuses base-R verb names (`subset()`, `merge()`, `count()`,
`transform()`, `split()`) on purpose, and deliberately does **not** ship
dplyr-named verbs, so it can be attached next to `dplyr` without shadowing its
grammar. It still shares the true base-R names with other packages, so if you
`library(data.table)` in the same session, whichever attaches **last** wins
for a shared name. Two fixes:

- call it explicitly: `basetable::transform(...)`;
- or `conflicted::conflict_prefer("transform", "basetable")` once per session.

## Operation dictionary

| Family | Exported functions | Base reference |
| --- | --- | --- |
| Row subsetting | `subset()` | `base::subset()` |
| Column keep / drop / rename | `pick()`, `drop()`, `renamecols()` | `[`, `names<-()` |
| Transformation | `transform()`, `within()` | base equivalents |
| Ordering | `orderrows()` | `order()` |
| Distinct / duplicates | `uniquerows()`, `duplicaterows()`, `removeduplicates()` | `unique()`, `duplicated()` |
| Aggregation | `aggregate()`, `count()`, `summaries()` | `aggregate()`, `table()` |
| Joins | `merge()`, `semimerge()`, `antimerge()`, `updatemerge()`, `crossmerge()`, `nonequimerge()`, `overlapmerge()`, `rangemerge()`, `rollingmerge()` | `merge()` |
| Row / column bind | `rbindfill()` | `rbind()` |
| Split / apply | `split()`, `applyby()` | `split()` |
| Reshaping | `tolong()`, `towide()`, `reshape()`, `stack()`, `unstack()` | base equivalents |
| Completion | `completegrid()` | `expand.grid()` + join |
| File I/O | `btread()`, `btwrite()`, `bt_aggregate()`, `bt_count()`, `bt_distinct()`, `bt_freq()` | `read.delim()` / fused file→result |
| Inspection | `glimpse()`, `dims()`, `types()`, `headtail()` | `str()`, `dim()`, `head()` |
| EDA | `describe()`, `missingness()`, `profile()`, `freq()`, `summarytab()`, `compare()` | base summaries |

`btread()` memory-maps the file and, with `lazy = TRUE`, returns columns as
ALTREP vectors parsed on first access; `bt_aggregate(file, ...)` and friends
fuse the parse with the grouping so unused columns are never materialised.

## Status

Every exported function has direct test coverage. Vignettes cover getting
started, data manipulation, exploration, a complete function reference, and
benchmarks. CI checks release R on Linux, macOS and Windows plus oldrel and
devel.
