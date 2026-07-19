# basetable 0.5.0

Initial development cycle covering performance, correctness, documentation,
and CRAN readiness.

## Performance

* Closed the largest wrapper-overhead gaps against the data.table backend.
  `rollingmerge()` reimplemented a rolling/nearest join with a hand-rolled
  per-group R loop that measured at roughly 1500x the cost of data.table's
  native `roll=` join; it now runs at parity.
* `count()`, `duplicated_keys()`, `freq()`, `filldown()`, `fillup()`,
  `split()`, and `summarise()`/`summarize()`/`summaries()` were rewritten to
  route through data.table's native grouping instead of base-R
  `interaction()`/`table()`/`split()` idioms.
* Added `inst/benchmarks/benchmark-all.R` as a standing benchmark harness
  tracking basetable against raw data.table.

## Correctness

* Audited every one of the 250+ exported functions and added missing test
  coverage (all exported functions now have direct tests).
* Fixed real bugs found along the way, including: `floordate()`/
  `ceilingdate()`/`rounddate()` were no-ops; `denserank()` ranked by
  first-appearance order instead of sorted value; `padleft()`/`padright()`
  ignored their `pad` argument; `datediff()` ignored `units`; `dateseq()`
  errored whenever `length.out` was supplied; `parsedatetime()` dropped its
  requested time zone; `expandlevels()` prepended instead of appended new
  levels; `emptycols()` crashed on more than one row; `towide()`'s reshape
  formula was backwards; `nonequimerge()`/`rangemerge()` silently performed a
  plain equi-merge despite their names; `removeduplicates(keep = "none")`
  deleted every row instead of keeping non-duplicates; `equalrows(by =)`
  compared only the key columns; `changedrows()` had no actual
  change-detection; `changedcols()` accepted a `by` argument it never used;
  `missingness()` used a narrower "missing" definition than its sibling
  functions; `antimerge()`/`semimerge()`/`nonequimerge()` did not validate an
  empty `by`, unlike the rest of the merge family.
* Found and fixed three functions that were fully implemented and documented
  but never exported: `distinct()`, `denserank()`, `completegrid()`.

## Documentation

* Added `vignettes/functions-reference.qmd`, a Quarto-authored PDF reference
  vignette walking through every exported function with a real, runnable
  example, organized into 14 progressive sections.
* Repositioned the package's messaging around its actual goal: a teaching
  tool and base-R-to-data.table migration bridge, not a dplyr/data.table
  competitor. Documented the naming collisions this implies with
  dplyr/data.table and how to work around them.

## CRAN readiness

* `R CMD check --as-cran` passes with 0 errors/warnings (only the expected
  "New submission" NOTE).
* Added `cran-comments.md`, `CONTRIBUTING.md`, `URL`/`BugReports` fields.
