# CRAN submission comments: basetable 1.4.6

## Release summary

This release (the first since 1.4.2) adds nested-table helpers, improves
performance, and fixes correctness issues in joins, aggregation, row binding,
reshaping and `btread()`. Exported functions now use glued lowercase names;
the old underscore names remain available as deprecated aliases. See NEWS.md.

## Test environments

* Ubuntu 24.04, R 4.5.1 (local), `R CMD check --as-cran`

## Check results

0 errors | 0 warnings | 1 note

* The NOTE lists a non-portable compilation flag (`-mno-omit-leaf-frame-pointer`)
  that comes from the local R build configuration, not from the package.

## Reverse dependencies

We checked the 4 reverse dependencies on CRAN (funcml, mimar, missknn,
testflow) against this version. No new problems were found. The deprecated
aliases warn only when called directly by the user, so reverse dependencies
that still use the old names are unaffected.
