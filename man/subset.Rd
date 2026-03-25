\name{subset}
\alias{subset}
\alias{pick}
\alias{drop}
\alias{transform}
\alias{within}
\alias{reorder}
\alias{aggregate}
\alias{count}
\alias{merge}
\alias{split}
\alias{by_apply}
\alias{combine}
\alias{reshape}
\alias{stack}
\alias{unstack}
\title{Core data manipulation operations}
\description{
Base-faithful table manipulation helpers built around explicit arguments and
ordinary data-frame returns.
}
\usage{
subset(data, subset = NULL, select = NULL, drop = FALSE)

pick(data, cols)

drop(data, cols)

transform(data, ..., .keep = TRUE)

within(data, expr)

reorder(data, by, decreasing = FALSE, na.last = TRUE)

aggregate(data, by, value = NULL, fun, ..., na.rm = FALSE, sort = TRUE)

count(data, by, sort = TRUE, name = "n")

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
  \item{drop, keep.by, bind, sort, all, all.x, all.y, na.last, .keep, na.rm}{Control flags.}
  \item{...}{Additional expressions or arguments passed through to helpers.}
  \item{fun}{A function used for grouped summaries or split/apply work.}
  \item{name, id, suffixes, direction, form, decreasing}{Additional operation-specific controls.}
}
\value{
Most functions return a plain \code{data.frame}. \code{split()} returns a named
list and \code{combine()} binds compatible list elements back into a data
frame.
}
\examples{
subset(mtcars, cyl == 6, select = c("mpg", "hp"))
pick(mtcars, c("mpg", "hp"))
drop(mtcars, c("vs", "am"))
transform(mtcars, power = hp / wt)
aggregate(mtcars, by = "cyl", value = "mpg", fun = mean)
count(iris, by = "Species")
}
