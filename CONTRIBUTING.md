# Contributing to basetable

Thanks for considering a contribution. This project is a compact R package,
so the process is intentionally lightweight.

## Reporting a bug

Open an issue with:

- a minimal reproducible example (a few lines of R, ideally using a built-in
  dataset such as `mtcars` or `iris`)
- what you expected to happen and what happened instead
- your `sessionInfo()` output

## Proposing a change

1. Open an issue first for anything beyond a small fix, so the approach can
   be agreed on before you spend time on it.
2. Fork the repository and create a branch for your change.
3. Keep the base-R-faithful interface in mind: new functions should read like
   `subset()`, `merge()`, `aggregate()`, etc., not introduce a new grammar.
4. Prefer routing table operations through `data.table` (via
   `bt_as_data_table_ro()`/`bt_as_data_table()` in `R/helpers-internal.R`)
   rather than looping in base R; see `inst/benchmarks/benchmark-all.R` for
   the kind of overhead this avoids.
5. Add or update tests under `tests/testthat/` for any behavior change.
6. Run `devtools::test()` and `R CMD check` locally before opening a pull
   request; both should be clean.
7. Add a roxygen2 block (`#'` comments) above any new exported function, then
   run `roxygen2::roxygenise(".", roclets = "rd")` to regenerate its `.Rd`
   page.

## Pull requests

- One logical change per pull request.
- Describe *why* the change is needed, not just what changed.
- Link the issue it addresses, if any.

## Code style

- Match the existing style in the file you're editing (explicit arguments,
  no non-standard evaluation beyond what the base-R equivalents already use).
- No unnecessary abstractions: a small, direct fix is preferred over a
  generalized one.
