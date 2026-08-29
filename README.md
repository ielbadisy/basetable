# basetable

<!-- badges: start -->
[![R-CMD-check](https://github.com/ielbadisy/basetable/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ielbadisy/basetable/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

`basetable` is a fast in-memory data-manipulation package for R with a
**base-R interface** and **no dependencies**. You write `subset()`,
`transform()`, `aggregate()`, `merge()`, `split()`, and the work runs on the
package's own C++ engine. There is no `data.table`, no `dplyr`, no Arrow
underneath, just `parallel`, `stats`, `stringi`, `utils`.

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
allocates far less on grouped operations. At 1e6 rows on one Linux machine
with 8 threads (`inst/benchmarks/benchmark-scale.R`), time relative to
`data.table` (lower is faster; **bold** = basetable ahead):

| Operation | few groups | many groups |
| --- | ---: | ---: |
| `uniquerows()` / `distinct()` | **0.8x** | **0.55x** |
| `aggregate(fun = sd)` | **0.56x** | **0.75x** |
| grouped `count()` | 1.0x | **0.66x** |
| `aggregate(fun = mean)` | 1.0x | **0.9x** |
| `semimerge()` | 0.9x | **0.24x** |
| `pick()` / projection | **0.65x** | **0.5x** |
| `merge()` (equi) | **0.9x** | 1.0x |
| `rbindfill()` | **0.9x** | 1.2x |
| `subset(x > c)` | 1.1x | 1.0x |
| `subset(x > a & y < b)` | 1.4x | 1.5x |
| `orderrows()` (string key) | 2.5x | 2.0x |

Grouped reducers accumulate in C++ without materialising intermediate
columns, so a grouped `aggregate` or `count` allocates near zero where the
other engines allocate tens of megabytes. The heavier operations
(aggregation, sort pipeline, filter, join probe) use multiple threads via
`setthreads()`.

The remaining gap is **sorting**: `orderrows()` uses a stable parallel radix
over integer codes and is ~20x faster than base `order()`, but still ~2-2.5x
of `data.table`, whose hand-tuned parallel radix is the one operation
basetable does not yet match. `collapse` and `polars` are also faster on
several columnar and grouped paths; matching their parallel-radix / Arrow
engines is not a near goal.

The `Benchmarks` vignette has the full reproducible report against base R,
`data.table`, `dplyr` and `collapse`.

## Positioning

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
| File I/O | `btread()`, `btwrite()`; `aggregate()` / `count()` / `distinct()` / `freq()` also take a file path | `read.delim()`, fused file to result |
| Inspection | `glimpse()`, `dims()`, `types()`, `headtail()` | `str()`, `dim()`, `head()` |
| EDA | `describe()`, `missingness()`, `profile()`, `freq()`, `summarytab()`, `compare()` | base summaries |

`btread()` memory-maps the file and, with `lazy = TRUE`, returns columns as
ALTREP vectors parsed on first access. `aggregate()`, `count()`, `distinct()`
and `freq()` accept a single file path as their first argument and fuse the
parse with the grouping, so unused columns are never materialised.

## Status

Every exported function has direct test coverage. Vignettes cover getting
started, data manipulation, exploration, a complete function reference, and
benchmarks. CI checks release R on Linux, macOS and Windows plus oldrel and
devel.
