# CRAN submission comments: basetable 1.4.9

## Release summary

This is an early update (1.4.6 was published on 2026-11-13) to fix results
that were wrong without any warning:

* Grouped `var()` and `sd()` in `aggregate()` used a one-pass formula that
  lost precision when the mean was large relative to the spread. For values
  near 1e8, it returned NaN. Version 1.4.9 uses a shifted Welford update with a
  pairwise merge, which matches an exact two-pass computation to about 1e-15.
* Columns read with `btread(lazy = TRUE)` made most verbs fail.
* Calls written for the base, stats and utils functions that basetable masks
  (`subset()` on a vector, `split(df, df$g)`, `profile()` on a fitted model,
  and so on) now go to those functions instead of failing.

See NEWS.md.

## Test environments

* Ubuntu 24.04, R 4.5.1 (local), `R CMD check --as-cran`
* GitHub Actions: Ubuntu (R devel, release, oldrel-1), macOS (release),
  Windows (release)

## Check results

0 errors | 0 warnings | 2 notes

* Days since last update: this update fixes the wrong results listed above.
* The NOTE about a non-portable compilation flag (`-mno-omit-leaf-frame-pointer`)
  comes from the local R build configuration, not from the package.

## Reverse dependencies

We checked the 6 reverse dependencies on CRAN (CEACT, funcml, mimar, missknn,
survalis, testflow) against this version. No new problems were found.
