# CRAN submission comments: basetable 1.4.2

## Note to CRAN

Bug-fix release. `split()` unconditionally treated its first argument as a
table, so attaching basetable broke the base R idiom
`split(vector, factor)` for any non-data-frame input. `split()` now checks
`is.data.frame()` and dispatches to `base::split()` when the input is not a
table, matching base R's behavior for vectors while keeping the existing
table-splitting behavior unchanged.

## Test environments

* Local: Ubuntu 24.04, R 4.5.1
* GitHub Actions: Ubuntu 22.04 (release, devel, oldrel-1), macOS (release),
  Windows (release)

## R CMD check results

0 errors | 0 warnings | 3 notes

* `Days since last update: 3` -- this is a quick bug-fix resubmission
  following a regression found while writing user-facing documentation.
* `checking compilation flags used ... NOTE` (`-mno-omit-leaf-frame-pointer`)
  comes from the maintainer's personal `~/.R/Makevars`, not from the
  package's own `src/Makevars`, and does not appear on the CI builders.
* `unable to verify current time` is a local sandbox artifact (no network
  access to a time server), not related to the package.

## Release summary

Patch release fixing a `split()` regression; no other functional changes.
See NEWS.md.

## Downstream dependencies

There are currently no downstream dependencies for this package.
