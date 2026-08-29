# basetable C++ engine roadmap

This records the remaining native-engine work after the first in-memory C++
kernels. Public R function names stay stable; R remains responsible for
base-style argument handling and NSE until expression compilation exists.

## Implemented

- Native projection and row materialisation for `pick()`, `drop()`, `subset()`,
  `firstrows()`, `lastrows()`, and `reverse()`.
- Native ordering for `orderrows()`, with character key columns pre-ranked to
  integers so the comparator never runs `strcmp`.
- Native distinct and duplicate masks for `uniquerows()`, `duplicaterows()`,
  and `removeduplicates()`, including dense integer/logical fast paths.
- Native grouped counts for `count()`.
- Integer key codec for grouping / distinct / duplicates / joins: numeric
  columns folded into one domain, character and factor dictionary encoded and
  matched by label, joins sharing one dictionary across build and probe; a
  single character/factor group column has a dedicated dense path.
- Native in-memory grouped reducers for `aggregate()` when `fun` is `sum`,
  `mean`, `min`, `max`, `var`, `sd`, `"n"`, or `"length"`.
- Native semi/anti membership masks for `semimerge()` and `antimerge()`.
- Native equi-join materialisation (`bt_join_`) and first-match index
  (`bt_first_match_`) for `merge()`, `crossmerge()`, `completegrid()`, and
  `updatemerge()`.
- Native predicate join (`bt_range_join_`, equi keys plus `<`/`<=`/`>`/`>=`/`==`)
  for `nonequimerge()`, `overlapmerge()`, and `rangemerge()`. When every
  condition bounds one shared numeric y column the bucket is sorted once and
  the match window is found by binary search.
- Native rolling join (`bt_rolling_join_`, backward/forward/nearest with
  tolerance) for `rollingmerge()`, each bucket sorted once and searched by
  binary search.
- Native expression kernel (`bt_expr_`) for `subset()` predicates: arithmetic,
  comparison, boolean (three-valued), unary minus, and `ifelse()`, with an
  automatic `eval()` fallback for anything unsupported.
- Native row-bind (`bt_rbind_`) with column union, NA fill and type promotion
  for `rbindfill()`, `applyby(bind = TRUE)` and long reshape.
- `basetable` result class stamped directly by the engine (`set_table_class`);
  no `data.table` class, no `data.table` dependency anywhere.
- File-native `btread()` / `btwrite()`; `aggregate()`, `count()`,
  `distinct()` and `freq()` accept a file path and fuse parse with group-by.

## Next engine tracks

- Interval-overlap joins (`overlapmerge`, two-y-column non-equi conditions)
  still scan the bucket; add an interval-tree or sweepline path.
- `orderrows()` radix passes run on threads, but the codes pass, the
  inter-column gather and the final materialise are serial, so the speedup
  caps around 1.5x. A cache-blocked parallel radix over the whole pipeline is
  what would match `data.table`.
- Parallelise the rest: the `group_single` dictionary pass, the dense
  integer-key aggregate path, `bt_group_id_`, `bt_count_`, and join build /
  probe. (Grouped `aggregate()` with a dense code path already reduces in
  parallel above ~750k rows.)
- Extend the expression kernel to `transform()`: needs integer-result
  preservation (int op int stays int) and a grouped variant so
  `transform(by =)` stops looping groups in R.
- Add thread-aware kernels for row copying, dense grouping, hash aggregation,
  and sort partitioning.
- Move from a `data.table` compatibility output class toward an owned table
  class once the join/reshape/summarise surface has native parity.
