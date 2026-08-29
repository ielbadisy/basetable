# CRAN submission comments: basetable 1.0.0

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

This is a major release. `basetable` no longer depends on `data.table`. Every
operation now runs on a native C++ engine bundled with the package:
projection, filtering, ordering, distinct and duplicate detection, grouping,
grouped reducers, all join kinds, row-binding and `subset()` predicate
evaluation are compiled `.Call` kernels, several of them multi-threaded via
`setthreads()`. Results carry a light `basetable` S3 class over an ordinary
data frame, with `print`, `[`, `as.data.frame` and `as.list` methods.

`data.table`, `dplyr` and `collapse` remain in `Suggests` only, as
competitors in the benchmark vignette.

## Downstream dependencies

There are currently no downstream dependencies for this package.
