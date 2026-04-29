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
