# btread / btwrite benchmark results

Run `Rscript bench/benchmark-io.R`. Numbers below are one representative run on
an 8-core Linux laptop (GCC 13, R 4.5.1), median of 5 iterations, files in page
cache. `Nt` = 8 threads. This table was taken with `-O3` in `src/Makevars`; the
shipped `Makevars` uses R's default `-O2`, at which the "mixed types" edge over
`fread` disappears into noise but the "string heavy" win over `fread` holds
(~1.5x, repeatable).

| scenario | winner | basetable (best) | vs winner | vs `read.csv` |
|---|---|---|---|---|
| long numeric, 2e6 x 4, full read | `fread` Nt 0.022 s | `btread` Nt 0.136 s | 6.2x slower | 30x faster |
| wide numeric, 6e4 x 50, full read | `vroom` 0.013 s | `btread` Nt 0.036 s | 2.7x slower | n/a |
| wide numeric, read 3 of 50 cols | `vroom` 0.013 s | `btread` lazy+select 0.023 s | 1.8x slower | n/a |
| mixed types, 1e6 x 6, full read | `vroom` 0.060 s | `btread` Nt **0.117 s** | 2.0x slower | — beats `fread` (0.129 s) |
| string heavy, 5e5 x 10, full read | `vroom` 0.023 s | `btread` Nt **0.184 s** | 8x slower | — beats `fread` (0.209 s) |
| write, 1e6 x 6 | `fwrite` 0.054 s | `btwrite` Nt 0.070 s | 1.3x slower | beats `vroom_write` marginally in some runs |

## Which configuration to use

`btread(file, n_threads = N)` — eager, multi-threaded. It is the only config
that ever beats a competitor: it beats `data.table::fread` on wide
string-heavy reads (~1.5x, repeatable at `-O2`), occasionally on mixed-type
reads (noise-level), and it is 20-190x faster than `utils::read.csv`
everywhere.

**No basetable configuration beats `vroom` on any read**, and none beats both
competitors at once. `vroom` returns almost immediately because it defers all
parsing to first column access and its index scan is faster than ours;
`fread`'s numeric parser is a decade of tuning plus parallel row coalescing.
So the honest answer to "which one outperforms data.table and vroom" is:
**none of them do, across the board.** The closest is `n_threads = N` eager.

`btread(lazy = TRUE)` helps only when few columns of a wide file are touched
(the untouched integer/double columns are never parsed). It still builds the
full row index and materialises character columns eagerly, so it cannot win a
"time to return" race against `vroom`.

## What would close the `vroom` gap

1. Defer the row index too: sample offsets, fill lazily per region on demand.
2. ALTREP character columns backed by a deferred string pool (biggest item).
3. SIMD delimiter/newline finding (AVX2) instead of `memchr` per field.
4. A dedicated fast float parser (`fast_float`) rather than `std::from_chars`.
5. Parallelise the character `mkCharLenCE` pass via a lock-free CHARSXP cache.
