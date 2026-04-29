\name{subset}
\alias{subset}
\alias{pick}
\alias{drop}
\alias{transform}
\alias{within}
\alias{reorder}
\alias{aggregate}
\alias{arrange}
\alias{bind_cols}
\alias{bind_rows}
\alias{count}
\alias{distinct}
\alias{filter}
\alias{merge}
\alias{mutate}
\alias{split}
\alias{by_apply}
\alias{combine}
\alias{relocate}
\alias{rename}
\alias{reshape}
\alias{select}
\alias{slice}
\alias{stack}
\alias{summarise}
\alias{summarize}
\alias{transmute}
\alias{unstack}
\title{Core data manipulation operations}
\description{
Base-faithful table manipulation helpers built around explicit arguments and
tibble returns.
}
\usage{
subset(data, subset = NULL, select = NULL, drop = FALSE)

pick(data, cols)

drop(data, cols)

transform(data, ..., .keep = TRUE)

mutate(data, ..., .keep = TRUE)

transmute(data, ...)

within(data, expr)

reorder(data, by, decreasing = FALSE, na.last = TRUE)

arrange(data, by, decreasing = FALSE, na.last = TRUE)

aggregate(data, by, value = NULL, fun, ..., na.rm = FALSE, sort = TRUE)

count(data, by, sort = TRUE, name = "n")

filter(data, ...)

select(data, cols)

rename(data, ...)

summarise(data, ..., by = NULL)

summarize(data, ..., by = NULL)

distinct(data, cols = NULL, .keep_all = FALSE)

slice(data, rows)

relocate(data, cols, .before = NULL, .after = NULL)

bind_rows(..., id = NULL)

bind_cols(...)

merge(x, y, by = NULL, all = FALSE, all.x = all, all.y = all,
      sort = FALSE, suffixes = c(".x", ".y"))

split(data, by, drop = FALSE, keep.by = FALSE)

by_apply(data, by, fun, ..., bind = FALSE, id = ".group")

combine(x, id = NULL)

reshape(data, ..., direction)

stack(data, select = NULL, drop = FALSE)

unstack(data, form, ...)
}
\arguments{
  \item{data, x, y}{A \code{data.frame} or \code{data.table}.}
  \item{subset, expr}{A base-style expression evaluated against columns.}
  \item{select, cols, by, value}{Character vectors naming columns.}
  \item{rows}{Integer row positions.}
  \item{drop, keep.by, bind, sort, all, all.x, all.y, na.last, .keep, na.rm, .keep_all}{Control flags.}
  \item{...}{Additional expressions or arguments passed through to helpers.}
  \item{fun}{A function used for grouped summaries or split/apply work.}
  \item{name, id, suffixes, direction, form, decreasing, .before, .after}{Additional operation-specific controls.}
}
\value{
Most functions return a tibble. \code{split()} returns a named list of tibbles
and \code{combine()} binds compatible list elements back into a tibble.
}
\examples{
subset(mtcars, cyl == 6, select = c("mpg", "hp"))
pick(mtcars, c("mpg", "hp"))
drop(mtcars, c("vs", "am"))
transform(mtcars, power = hp / wt)
filter(mtcars, cyl == 6, mpg > 18)
summarise(mtcars, mean_mpg = mean(mpg), by = "cyl")
aggregate(mtcars, by = "cyl", value = "mpg", fun = mean)
count(iris, by = "Species")
}
