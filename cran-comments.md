# CRAN submission comments — basetable 0.6.0

## Test environments

* Local: Ubuntu 24.04, R 4.5.1

## R CMD check results

0 errors | 0 warnings | 0 notes

`R CMD check --as-cran` (via `devtools::check()`), including vignette
re-building, is fully clean.

## Summary of changes since the last submission

This is a breaking-change release: `filter()`, `select()`, `rename()`,
`arrange()`, `mutate()`, `transmute()`, `summarise()`/`summarize()`,
`distinct()`, `slice()`, `relocate()`, `bind_rows()`, and `bind_cols()` have
been removed. These duplicated dplyr's verb names exactly, so attaching
`basetable` and `dplyr` in the same session could silently mask either
package's verb grammar depending on load order. The underlying
functionality remains available under base-flavored names that already
existed (`subset()`, `pick()`, `reorder()`, `transform()`, `summaries()`,
`move()`, `rbindfill()`) plus two new ones for functionality that had no
prior equivalent (`renamecols()`, `uniquerows()`).

Also added: a `by` argument on `transform()` for per-group column
computation, and real `typeconflict` handling in `rbindfill()`.

## Downstream dependencies

There are currently no downstream dependencies for this package.
