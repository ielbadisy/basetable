# basetable C++ engine roadmap

State of the native engine as of 1.0.0, and the work left to do. Public R
function names stay stable; R keeps base-style argument handling and NSE
capture, the C++ kernels do the computation.

## Implemented

- Native projection and row materialisation for `pick()`, `drop()`,
  `subset()`, `firstrows()`, `lastrows()`, and `reverse()`.
- Native ordering for `orderrows()`: a stable multi-column LSD radix over
  order-preserving integer codes (character columns dictionary-ranked in
  `strcmp` order), with the radix byte passes, code generation, inter-column
  gather, mask compaction and final materialise all threaded.
- Fused one-pass filter (`bt_filter_`) for the common `subset()` shapes --
  one `col <op> scalar` comparison over a numeric column, or two joined by
  `&` -- evaluated and materialised in a single threaded pass with no
  intermediate logical vector. Other predicate shapes fall back to
  `bt_expr_` plus a threaded stream-compaction of the mask.
- Native distinct and duplicate masks for `uniquerows()`, `duplicaterows()`,
  and `removeduplicates()`, including dense integer/logical fast paths.
- Native grouped counts for `count()`.
- Integer key codec for grouping / distinct / duplicates / joins: numeric
  columns folded into one domain, character and factor dictionary encoded and
  matched by label, joins sharing one dictionary across build and probe; a
  single character/factor group column has a dedicated dense path.
- Native in-memory grouped reducers for `aggregate()` when `fun` is `sum`,
  `mean`, `min`, `max`, `var`, `sd`, `"n"`, or `"length"`. Cardinality-aware
  dispatch: for few groups a fused per-thread local-dictionary reducer, for
  many groups `group_single` plus a parallel reduce above ~750k rows.
- Native semi/anti/update membership masks for `semimerge()`, `antimerge()`
  and `updatemerge()`, with a parallel probe.
- Native equi-join materialisation (`bt_join_`) and first-match index
  (`bt_first_match_`) for `merge()`, `crossmerge()`, `completegrid()`, and
  `updatemerge()`.
- Native predicate join (`bt_range_join_`, equi keys plus
  `<`/`<=`/`>`/`>=`/`==`) for `nonequimerge()`, `overlapmerge()`, and
  `rangemerge()`. When every condition bounds one shared numeric y column the
  bucket is sorted once and the match window is found by binary search.
- Native rolling join (`bt_rolling_join_`, backward/forward/nearest with
  tolerance) for `rollingmerge()`, each bucket sorted once and searched by
  binary search.
- Native expression kernel (`bt_expr_`) for `subset()` predicates:
  arithmetic, comparison, three-valued boolean, unary minus, and `ifelse()`,
  with an automatic `eval()` fallback for anything unsupported.
- Native row-bind (`bt_rbind_`) with column union, NA fill, type promotion
  and shared-class preservation, plus a bulk memcpy path for columns that
  already match type across every input; used by `rbindfill()`,
  `applyby(bind = TRUE)` and long reshape.
- Owned `basetable` result class, stamped directly by the engine
  (`set_table_class`) with `print`, `[`, `as.data.frame` and `as.list`
  methods. No `data.table` class, no `data.table` dependency anywhere.
- File-native `btread()` / `btwrite()`; `aggregate()`, `count()`,
  `distinct()` and `freq()` accept a file path and fuse parse with group-by.

## Next engine tracks

Ordered by measured impact (see `make-readme-figures.R`, 1e6 rows).

### 1. Sort pipeline -- the one clear loss

`orderrows()` is ~2-3x of `data.table` on a string key. The radix byte
passes are threaded but memory-bandwidth bound, and an MSD attempt (August
2026) was slower because the copy-back doubled memory traffic. What would
close it: a cache-blocked parallel forward radix that partitions once by the
high byte, then sorts partitions independently in cache, so each pass is
local and the threads do not contend on one shared output buffer. This is a
full rewrite of `radix_pairs`, not an incremental change.

### 2. Equi-join throughput

`merge()` is only at parity with `data.table` and loses to `dplyr`'s hash
join (roughly 145 ms vs 130 ms vs 65 ms at 1e6 rows). The `bt_join_` build
and probe are single-threaded. Parallelise: partition the build side by key
hash into per-thread bins, probe in parallel, then materialise with the
existing threaded gather. The membership probe (`bt_match_mask_`) already
shows this works for semi/anti.

### 3. Parallelise the remaining grouping kernels

`group_single` dictionary pass, the dense integer-key aggregate path,
`bt_group_id_`, and `bt_count_` are still serial. `count` and `sd` already
beat `data.table` here; parallelising widens the margin and helps the
mid-cardinality range where the fused reducer does not engage.

### 4. Expression kernel for `transform()`

Only `subset()` predicates compile today. `transform()` needs
integer-result preservation (int op int stays int, not promoted to double)
and a grouped variant so `transform(by =)` stops looping groups in R.

### 5. Interval-overlap joins

`overlapmerge()` and two-y-column non-equi conditions still linear-scan the
bucket after the equi-key match. Add an interval-tree or sweepline path for
the case where two conditions bound a `[start, end]` range.
