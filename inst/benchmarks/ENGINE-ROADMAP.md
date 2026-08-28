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
- File-native `btread()`, `btwrite()`, `bt_aggregate()`, `bt_count()`,
  `bt_distinct()`, and `bt_freq()`.

## Next engine tracks

- Replace remaining serialized composite keys with typed composite key tables
  that compare raw int/double/string/factor slots directly.
- Add dictionary encoding for character columns so grouping, joins, distinct,
  and sorting can operate on integer codes.
- Replace the current hash-probe join kernels (string-serialised composite
  keys, O(build + probe) with per-key match lists) with sorted-merge and
  dictionary-encoded variants so range/non-equi joins stop scanning full key
  buckets and rolling joins use binary search.
- Add an expression kernel planner for common `transform()` and `subset()`
  expressions: arithmetic, comparisons, boolean operators, `ifelse`, and
  scalar recycling.
- Add thread-aware kernels for row copying, dense grouping, hash aggregation,
  and sort partitioning.
- Move from a `data.table` compatibility output class toward an owned table
  class once the join/reshape/summarise surface has native parity.
