# basetable 1.3.1

## Performance

* Equi joins (`merge()` and the inner/left cases behind it) now probe the
  build side in parallel and gather the atomic output columns on worker
  threads. On the 1e6-row reference join this moves `merge()` from roughly
  1.7x of `data.table` to about parity.

# basetable 1.3.0

## New features

* `casewhen(conditions, default = NA)`: vectorised multi-branch selection.
  `conditions` is a named list of equal-length logical vectors, the same
  shape as `collapsevalues()` -- the name of the first element that is `TRUE`
  at a position is the value taken there; positions matching nothing get
  `default`. Composes inside `transform()`.

# basetable 1.2.0

## New features

* `subset()` gains a `by` argument: the predicate is then evaluated within
  each group, so aggregate references in it (`mean(x)`, `max(x)`, ...) are
  per group. Kept rows are returned in their original order. This is
  basetable's explicit answer to a grouped filter -- there is still no
  stateful `group_by()`.
* `samplerows()` and `samplefrac()` gain a `by` argument: sample `n` rows
  (capped at the group size) or a fraction of each group. Sampled rows are
  returned in their original order.

# basetable 1.1.0

basetable is now a self-contained data-manipulation package with a base-R
interface and a bundled C++ execution engine. It has no external computation
dependency. This is the first CRAN release.

## Breaking changes

* **`data.table` is no longer a dependency.** Every verb runs on basetable's
  own compiled engine. `data.table` (with `collapse` and `dplyr`) remains in
  `Suggests` only, as a comparison target in the benchmark vignette.
* **Verbs return a `basetable`** rather than a `data.table`. A `basetable` is
  an ordinary data frame with one extra class so it prints compactly and `[`
  keeps the class and defaults to `drop = FALSE`. `as.data.frame()` returns a
  plain frame. The `as =` argument of `btread()` and the file readers now
  takes `"basetable"` in place of `"data.table"`.
* **The fused file readers lost their `bt_` prefix.** `aggregate()`,
  `count()`, `uniquerows()` and `freq()` now accept a single file path as
  their first argument and route to the one-pass scan, so there is one name
  per operation whether the input is a data frame or a delimited file.
  `bt_aggregate()`, `bt_count()`, `bt_distinct()` and `bt_freq()` are removed.
* **Names coined by other packages are avoided.** The string predicates
  `contains()` / `matches()` (tidyselect selection helpers) are now
  `containstext()` / `matchestext()`; `glimpse()` (tibble) is `preview()`;
  `cummean()` (dplyr) is `cumavg()`. `distinct()` is dropped -- `uniquerows()`
  is the data-frame form and also takes a file path. The only names shared
  with `dplyr` are now `count()` and `pick()`, both read as base-style verbs.

## Native C++ engine

* All the table-shape work now runs in registered `.Call` kernels:
  projection, row subsetting, ordering, distinct and duplicate detection,
  grouping, grouped reducers (`sum`/`mean`/`min`/`max`/`var`/`sd`/`n`), all
  join kinds (equi, semi, anti, update, cross, non-equi, range, rolling),
  row-bind, and `subset()` predicate evaluation. Custom aggregation
  functions still run as ordinary R closures per group.
* A composite key codec replaces the old byte-string keys used by grouping
  and joins: numeric columns fold into one value domain, character and factor
  columns are dictionary encoded and matched by label, and a join builds one
  dictionary that its probe side reuses read-only.
* `orderrows()` sorts with a stable multi-column radix over order-preserving
  integer codes (character columns ranked in `strcmp` order) instead of a
  comparison sort; ordering is unchanged (C-locale byte order).
* `subset()` compiles a supported predicate (column and scalar references;
  `+ - * / ^ %%`; comparisons; `& | !`; unary `-`; `ifelse()`) to a small
  stack machine, with an `eval()` fallback for anything outside that grammar.
* The heavier kernels use multiple threads, controlled by `setthreads()`:
  grouped aggregation, the sort pipeline, the filter mask compaction and
  column gather, and the join membership probe.
* Delimited-file reading is native (`btread()` / `btwrite()`, memory-mapped,
  RFC 4180, threaded parsing, optional ALTREP lazy columns), and
  `aggregate()` / `count()` / `uniquerows()` / `freq()` on a file path fuse the
  parse with the grouping so unused columns are never materialised.

## Performance

Measured against `data.table` at 1e6 rows on the reference machine (Linux, 8
threads); see the `Benchmarks` vignette for the full report.

* Faster than `data.table`: `uniquerows()`, `sd` / `var` by
  group at every cardinality, `count` and `mean` by group at high
  cardinality, `semimerge()` (parity at low cardinality down to ~0.25x at
  high), `pick()` / column projection, and `subset()` on the common
  `col <op> scalar` shapes (single or `&`-joined comparisons over a numeric
  column), now evaluated and materialised in one threaded pass with no
  intermediate logical vector.
* Roughly at parity: `merge()`, `mean` by group at low cardinality,
  `rbindfill()`.
* Slower than `data.table`: string and integer `orderrows()` (~2x; the
  radix byte passes are memory-bandwidth bound). `data.table`'s parallel
  radix sort is the one operation basetable does not yet match.
* Grouped reducers accumulate in C++ without materialising intermediate
  columns, so a grouped `aggregate()` or `count()` allocates near zero where
  the other engines allocate tens of megabytes.

## New features

* `uniquerows()` also takes a single file path: a fused one-pass scan that
  returns the distinct combinations of `cols` without materialising the file.
* `rbindfill()` bulk-copies columns that already share a type across every
  input and now keeps a shared column class such as `Date`.
* `stack()` is exported and consistently returns a `basetable`, masking
  `utils::stack()` like the rest of the base-flavored API.
* `inst/benchmarks/benchmark-scale.R`: a size x cardinality harness against
  base R, `data.table`, `dplyr` and `collapse`.

# basetable 0.8.1

## New features

* `btread(lazy = TRUE)` now returns **character** columns as ALTREP too (not
  only integer/double), so a lazy read parses nothing until a column is
  touched. Combined with the parallel row indexer this makes "open a file"
  and "read a few columns of a wide file" faster than both `data.table::fread`
  and `vroom` (`vroom` still wins nothing here; `fread` keeps the lead only
  when every value is parsed with all threads). See `bench/RESULTS.md`.

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

* `subset(data, select = -colname)`, negative bare-symbol column selection,
  a base R `subset()` idiom, errored with "object 'colname' not found"
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
