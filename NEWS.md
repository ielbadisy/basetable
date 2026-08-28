# basetable 0.8.0

## New features

* `btread()` and `btwrite()`: a from-scratch delimited-file reader and
  writer implemented in C++ (`src/`). The reader memory-maps the file, scans
  it once for row boundaries with RFC 4180 quoting, guesses column types from
  a bounded sample, and materialises typed vectors with numeric columns
  filled by a `std::thread` pool. `btread(lazy = TRUE)` returns integer and
  double columns as ALTREP vectors that parse their column only on first
  access, which pairs well with `col_select` for wide files. `btwrite()`
  formats disjoint row ranges into private buffers in parallel. Supports
  delimiter sniffing, `col_types`, `col_select`, `na`, `comment`, `skip`,
  `n_max`, `trim_ws`, and `.gz` input. See `bench/benchmark-io.R`.

# basetable 0.7.1

## Bug fixes

* `subset(data, select = -colname)` — negative bare-symbol column selection,
  a base R `subset()` idiom — errored with "object 'colname' not found"
  instead of dropping the column. `select` was being force-evaluated as an
  ordinary argument (via `is.null(select)`/`is.character(select)`) before
  its NSE form ever got a chance to run, and the column-index mask used to
  evaluate NSE selections was unnamed. Rewritten to defer evaluation and
  build a proper named index mask, matching `select`/`subset` and
  `select = -c(a, b)` style multi-column negative selection too.

# basetable 0.7.0

## Breaking changes

* Removed `reorder()`. It was an exact duplicate of `orderrows()` (added in
  0.6.0 to replace the dplyr-named `arrange()`), and it masked
  `stats::reorder()` the same way the removed dplyr verbs masked dplyr's,
  which silently broke `ggplot2::aes(reorder(...))` for anyone with
  `basetable` attached. Use `orderrows()` instead.
* `split()`'s `keep.by` now defaults to `TRUE` (was `FALSE`), so the
  grouping column is kept in each piece by default, matching base R's
  `split()` behavior on data frames. Pass `keep.by = FALSE` for the old
  default.

## Bug fixes

* `aggregate()` no longer forces `na.rm` onto every `fun`. A custom
  summary function with no `na.rm` argument (and no `...`) previously
  errored with "unused argument"; `na.rm` is now only forwarded when
  `fun` actually accepts it.

# basetable 0.6.0

## New features

* `transform()` gained a `by` argument. When supplied, each expression in
  `...` is evaluated within every group instead of across the whole table
  (e.g. `transform(data, dev = x - mean(x), by = "g")` computes the
  deviation from each group's own mean), so the result still has one row
  per input row. Use `summaries()` when you want one row per group instead.
* `rbindfill()`'s `typeconflict` argument now does something: `"error"`
  (the default) checks every shared column across inputs up front and stops
  with a message naming the column and the conflicting types before any
  coercion happens; `"coerce"` skips the check and falls back to
  `data.table::rbindlist()`'s usual coercion.

## Breaking changes

* Removed the dplyr-named wrapper verbs added in `R/verbs.R`: `filter()`,
  `select()`, `rename()`, `arrange()`, `mutate()`, `transmute()`,
  `summarise()`/`summarize()`, `distinct()`, `slice()`, `relocate()`,
  `bind_rows()`, and `bind_cols()`. These duplicated dplyr's verb names
  exactly, which meant attaching `basetable` and `dplyr` in the same session
  could silently mask either package's verb grammar depending on load
  order. The underlying functionality already existed under base-flavored
  names and continues to be exported: use `subset()`, `pick()`, `reorder()`,
  `transform()` (with `.keep = FALSE` in place of `transmute()`),
  `summaries()`, `move()`, and `rbindfill()` instead. `bind_cols()` had no
  package-specific behavior beyond base R's `cbind()`; use `cbind()`
  directly. Two functions had no existing base-flavored equivalent and are
  now available under new names: `renamecols()` (was `rename()`) and
  `uniquerows()` (was `distinct()`).

# basetable 0.5.4

## Portability

* Made `removeaccents()` and `transliterate()` deterministic across Linux,
  Windows, and macOS by using ICU transliteration instead of the
  platform-dependent `iconv(..., "ASCII//TRANSLIT")` behavior.
* Made the complete PDF function-reference vignette build in-process so it
  can load the package from `R CMD build`'s temporary library on every
  supported platform.

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
