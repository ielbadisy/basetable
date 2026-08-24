# basetable 0.5.3

## Evaluation semantics

* `transform()`, `mutate()`, and `transmute()` now resolve ordinary values
  from their true calling function, including when used inside a helper.
* Fixed transform assignment when an input column is literally named
  `value`; the evaluated expression result is now assigned through a
  standard-evaluation data.table setter.

## Performance

* Removed avoidable data.frame round-trips from `filter()`, `rename()`,
  `distinct()`, `slice()`, `relocate()`, and `bind_rows()`.
* Removed duplicate return copies from transformation and ordering paths.
* Reduced `filter()` to one logical mask in the common one-expression case
  and avoided allocating an NA mask when no missing values are present.
* Added a reproducible runtime and allocated-memory benchmark at 100,000,
  1 million, and 10 million rows. At 10 million rows, filtering, grouped
  counting, and mixed ordering reach allocation parity with the equivalent
  raw data.table paths on the reference system.

## Documentation

* Expanded the data-manipulation vignette with compact pipeline, mixed-order,
  caller-scope, validation, grouped-summary, and namespace examples.
* Defined the canonical compact core and the intentional in-memory package
  boundary in the getting-started vignette.
* Documented scale benchmark methodology and reference results.

# basetable 0.5.2

## Correctness and performance

* Fixed mixed-direction multi-column ordering in `orderrows()` and every
  helper that uses the shared base ordering path.
* Routed `orderrows()` through the package's data.table-backed ordering
  implementation, removing an avoidable data.frame conversion while
  preserving input immutability.
* Added regression coverage for per-column sort directions.

# basetable 0.5.1

## CRAN submission prep

* Removed the hand-written `Author:`/`Maintainer:` fields from `DESCRIPTION`
  (they had drifted out of sync with `Authors@R`, tripping an `R CMD check
  --as-cran` NOTE); both are now derived from `Authors@R` as usual.
* Added a GitHub Actions `R-CMD-check.yaml` workflow (macOS/Windows/Ubuntu,
  release/devel/oldrel-1) and matching README badges.
* `R CMD check --as-cran` is clean apart from the two expected first-submission
  notes (new submission, unable to verify current time).

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
