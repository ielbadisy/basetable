# basetable operation dictionary

| Family | Intent | Exported function | Base relationship | Backend relationship | Why not dplyr-style naming |
| --- | --- | --- | --- | --- | --- |
| Inspection | Compact structure preview | `glimpse()` | Extends `str()` with tabular focus | Formats metadata after standardization | `glimpse()` is descriptive; `summarise()`-style names are unrelated |
| Inspection | Dimensions | `dims()` | Thin wrapper around `dim()` | No special backend logic needed | Avoids turning shape inspection into a verb grammar |
| Inspection | Column type summary | `types()` | Extends `class()` / `typeof()` | Reads normalized table metadata | Distinct from `across()`-style type tooling |
| Inspection | First and last rows | `headtail()` | Combines `head()` and `tail()` | Uses ordinary row slicing | More compact than ad hoc preview verbs |
| Inspection | Column summaries | `describe()` | Extends `summary()` | Computes summaries efficiently from normalized columns | Not a `summarise()` clone; oriented to EDA |
| Rows | Row filtering | `subset()` | Direct base analogue | Evaluates rows then subsets through `data.table` | Keeps base semantics instead of `filter()` |
| Columns | Keep columns | `pick()` | Mirrors positive column indexing | Validated column selection | `select()` is too strongly tied to dplyr |
| Columns | Drop columns | `drop()` | Mirrors negative column indexing | Set difference over names | `select(-x)` is a dplyr idiom; `drop()` is explicit |
| Transform | Add/modify columns | `transform()` | Direct base analogue | Mutable `data.table` assignment | Avoids `mutate()` grammar |
| Transform | Block mutation/removal | `within()` | Direct base analogue | Evaluate then rebuild | Base name is already correct |
| Order | Sort rows | `reorder()` | Extends `order()` to data frames | `setorderv()` handles multi-column ordering | Avoids SQL-flavored `arrange()` |
| Grouping | Group summaries | `aggregate()` | Direct base analogue | Group aggregation in `data.table` | Avoids `summarise()` / grouped objects |
| Grouping | Group counts | `count()` | Close to `table()` and row counts | `.N` in `data.table` | Kept as a narrow convenience, not a grammar anchor |
| Join | Merge tables | `merge()` | Direct base analogue | `merge.data.table()` | Keeps established join naming |
| Split/apply | Split data | `split()` | Direct base analogue | Group split in `data.table` | No grouped state object |
| Split/apply | Apply by groups | `by_apply()` | Inspired by `by()` | Split then apply | Explicitly grouped without `group_by()` |
| Split/apply | Recombine pieces | `combine()` | Natural complement to split | `rbindlist()` | Clear and base-like |
| Reshape | Wide/long reshape | `reshape()` | Direct base analogue | Delegates to `stats::reshape()` | Base already has the right term |
| Reshape | Stack columns | `stack()` | Direct base analogue | Delegates to `utils::stack()` | Base already has the right term |
| Reshape | Unstack values | `unstack()` | Direct base analogue | Delegates to `utils::unstack()` | Base already has the right term |
| Missingness | Missing summaries | `missingness()` | Extends `is.na()` / `complete.cases()` | Column/row scans after normalization | `missing()` would conflict with base R's primitive |
| EDA | Frequency tables | `freq()` | Extends `table()` / `prop.table()` | Counts in `data.table` | Focused helper, not a grouped summarise layer |
| EDA | Dataset profile | `profile()` | Extends `summary()` | Joins type, missingness, and distinct stats | EDA-oriented, not transformation-oriented |
| EDA | Compare datasets | `compare()` | Extends `all.equal()`-style checking | Compares schema and summaries | Makes differences explicit without join grammar |
