# CRAN submission comments: basetable 1.3.1

## Note to CRAN

This upload supersedes the earlier 0.9.0 submission, which was returned with
a manual review (K. Lauseker, 2026-08-21). Since then the package removed its
'data.table' dependency in favour of a bundled 'C++' engine and renamed a few
exports that clashed with other packages, so it is now at 1.3.1. Please
review this version in place of 0.9.0.

Both points from the 0.9.0 review are addressed:

* Software names in the Description are single-quoted ('C++', 'basetable').
* The vignettes no longer leave `options()` changed. `functions-reference`
  and `benchmarking` capture the prior values in their setup chunk and
  restore them in a teardown chunk (`options(.old_opts)` /
  `options(basetable.threads = .old_threads)`).

## Test environments

* Local: Ubuntu 24.04, R 4.5.1
* GitHub Actions: Ubuntu 22.04 (release, devel, oldrel-1), macOS (release),
  Windows (release)

## R CMD check results

0 errors | 0 warnings | 1 note

The one NOTE is local only:

```
checking compilation flags used ... NOTE
  Compilation used the following non-portable flag(s):
    '-mno-omit-leaf-frame-pointer'
```

This flag comes from the maintainer's personal `~/.R/Makevars`, not from the
package. The package's own `src/Makevars` sets no non-portable flags, so this
NOTE does not appear on the CI builders or on a clean toolchain.

CRAN's incoming checks will also raise the usual "New submission" NOTE.

## Release summary

First CRAN release. `basetable` depends on no external computation package.
Every operation runs on a native C++ engine bundled with the package:
projection, filtering, ordering, distinct and duplicate detection, grouping,
grouped reducers, all join kinds, row-binding and `subset()` predicate
evaluation are compiled `.Call` kernels, several of them multi-threaded via
`setthreads()`. Results carry a light `basetable` S3 class over an ordinary
data frame, with `print`, `[`, `as.data.frame` and `as.list` methods.

`data.table` and `dplyr` remain in `Suggests` only, as
competitors in the benchmark vignette.

## Downstream dependencies

There are currently no downstream dependencies for this package.
