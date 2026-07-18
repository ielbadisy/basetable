\name{subset}
\alias{subset}
\alias{transform}
\alias{reorder}
\alias{by_apply}
\alias{combine}
\alias{reshape}
\title{Core data manipulation operations}
\description{
Base-faithful table manipulation helpers built around explicit arguments and
tibble returns. See \code{\link{aggregate}}, \code{\link{arrange}},
\code{\link{bind_cols}}, \code{\link{bind_rows}}, \code{\link{count}},
\code{\link{distinct}}, \code{\link{drop}}, \code{\link{filter}},
\code{\link{merge}}, \code{\link{mutate}}, \code{\link{pick}},
\code{\link{relocate}}, \code{\link{rename}}, \code{\link{select}},
\code{\link{slice}}, \code{\link{split}}, \code{\link{stack}},
\code{\link{summarise}}, \code{\link{transmute}}, \code{\link{unstack}}, and
\code{\link{within}} for the remaining core verbs, each documented on its own
page.
}
\usage{
subset(data, subset = NULL, select = NULL, drop = FALSE)

transform(data, ..., .keep = TRUE)

reorder(data, by, decreasing = FALSE, na.last = TRUE)

by_apply(data, by, fun, ..., bind = FALSE, id = ".group")

combine(x, id = NULL)

reshape(data, ..., direction)
}
\arguments{
  \item{data, x}{A \code{data.frame} or \code{data.table}.}
  \item{subset}{A base-style expression evaluated against columns.}
  \item{select, by}{Character vectors naming columns.}
  \item{drop, bind, na.last, .keep, decreasing}{Control flags.}
  \item{...}{Additional expressions or arguments passed through to helpers.}
  \item{fun}{A function used for split/apply work.}
  \item{id, direction}{Additional operation-specific controls.}
}
\value{
Most functions return a tibble. \code{split()} returns a named list of
tibbles and \code{combine()} binds compatible list elements back into a
tibble.
}
\examples{
subset(mtcars, cyl == 6, select = c("mpg", "hp"))
transform(mtcars, power = hp / wt)
}
