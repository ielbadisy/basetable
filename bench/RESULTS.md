# btread / btwrite benchmark results

Run `Rscript bench/benchmark-io.R`. Representative run on an 8-core Linux
laptop (GCC 13, R 4.5.1), median of several iterations, files in page cache.
`Nt` = 8 threads. Shipped `Makevars` uses R's default `-O2`.

## Wide mixed file, 5e5 x 30 (20 numeric + 10 character), ~237 MB

| operation | btread | vroom | fread | winner |
|---|---|---|---|---|
| open file, touch nothing | **28 ms** (`lazy = TRUE`) | 47 ms (altrep) | 213 ms | **btread** |
| read 3 of 30 columns | **23 ms** (`lazy` + `col_select`) | 47 ms | 51 ms | **btread** |
| materialise every value | 363 ms (`n_threads = 8`) | 2340 ms | **219 ms** | fread |

Since 0.8.1 `lazy = TRUE` makes **character** columns ALTREP as well, so a
lazy read parses nothing up front. With the parallel row indexer, "open a
file" and "read a few columns" now beat both `fread` and `vroom`.

`fread` keeps the lead on the third row only: parsing every value of a
100+ MB file with a decade-tuned parallel parser is its home turf, and
btread's eager path (single-threaded `mkCharLenCE` for strings) is ~1.6x
behind there. `btread(lazy = TRUE)` then touching all columns is slower
still (~1.7 s) because each column materialises single-threaded on first
access -- use `n_threads = N` (eager) when you know you need everything.

## Which mode to use

- Exploring a file / need only some columns -> `btread(file, lazy = TRUE)`
  (optionally `col_select`). Fastest of the three tools.
- Need every value, immediately -> `btread(file, n_threads = N)` (eager).
  ~1.6x behind `fread` on pure numeric+string throughput, 20-190x ahead of
  `read.csv`.

## Older single-type numbers (full read, eager, -O3)

| scenario | winner | btread best |
|---|---|---|
| long numeric 2e6x4 | `fread` Nt 0.022 s | Nt 0.136 s |
| wide numeric 6e4x50 | `vroom` 0.013 s | Nt 0.036 s |
| string heavy 5e5x10 | `vroom` 0.023 s | Nt 0.13 s -- beats `fread` (0.21 s) |
| write 1e6x6 | `fwrite` 0.054 s | `btwrite` Nt 0.070 s |

## Remaining work to widen the lead

1. Parallel character materialisation (per-thread CHARSXP caches, merge on
   the R thread) -- closes the eager full-read gap to `fread`.
2. Parallel per-column materialisation for `lazy = TRUE` + touch-all.
3. Deferred / partial row index for the truly-open-only case.
4. A dedicated fast float parser (`fast_float`) over `std::from_chars`.
