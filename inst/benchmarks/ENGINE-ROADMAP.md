# basetable C++ engine roadmap

This records the remaining native-engine work after the first in-memory C++
kernels. Public R function names stay stable; R remains responsible for
base-style argument handling and NSE until expression compilation exists.

## Implemented

- Native projection and row materialisation for `pick()`, `drop()`, `subset()`,
  `firstrows()`, `lastrows()`, and `reverse()`.
- Native ordering for `orderrows()`.
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
  for `nonequimerge()`, `overlapmerge()`, and `rangemerge()`.
- Native rolling join (`bt_rolling_join_`, backward/forward/nearest with
  tolerance) for `rollingmerge()`.
- Native expression kernel (`bt_expr_`) for `subset()` predicates: arithmetic,
  comparison, boolean (three-valued), unary minus, and `ifelse()`, with an
  automatic `eval()` fallback for anything unsupported.
- Native row-bind (`bt_rbind_`) with column union, NA fill and type promotion
  for `rbindfill()`, `applyby(bind = TRUE)` and long reshape.
- `basetable` result class stamped directly by the engine (`set_table_class`);
  no `data.table` class, no `data.table` dependency anywhere.
- File-native `btread()`, `btwrite()`, `bt_aggregate()`, `bt_count()`,
  `bt_distinct()`, and `bt_freq()`.

## Next engine tracks

- Extend the key codec to `orderrows()` so sorting can compare integer codes
  instead of `strcmp` for character columns.
- Range / non-equi joins still scan a whole equi-key bucket per x row, and
  rolling joins scan the bucket linearly; add sorted-merge / binary-search
  variants on top of the integer key codec.
- Parallelise the dense grouping and hash-aggregation passes.
- Extend the expression kernel to `transform()`: needs integer-result
  preservation (int op int stays int) and a grouped variant so
  `transform(by =)` stops looping groups in R.
- Add thread-aware kernels for row copying, dense grouping, hash aggregation,
  and sort partitioning.
- Move from a `data.table` compatibility output class toward an owned table
  class once the join/reshape/summarise surface has native parity.
