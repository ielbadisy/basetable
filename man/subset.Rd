\name{subset}
\alias{subset}
\alias{transform}
\alias{reorder}
\alias{reshape}
\title{Core data manipulation operations}
\description{
Base-faithful table manipulation helpers built around explicit arguments and
data.table returns. See \code{\link{aggregate}}, \code{\link{count}},
\code{\link{drop}}, \code{\link{merge}}, \code{\link{move}},
\code{\link{pick}}, \code{\link{renamecols}}, \code{\link{split}},
\code{\link{stack}}, \code{\link{summaries}}, \code{\link{uniquerows}},
\code{\link{unstack}}, and \code{\link{within}} for the remaining core
verbs, each documented on its own page.
}
\usage{
subset(data, subset = NULL, select = NULL, drop = FALSE)

transform(data, ..., .keep = TRUE, by = NULL)

reorder(data, by, decreasing = FALSE, na.last = TRUE)

reshape(data, ..., direction)
}
\arguments{
  \item{data}{A \code{data.frame} or \code{data.table}.}
  \item{subset}{A base-style expression evaluated against columns.}
  \item{select, by}{Character vectors naming columns. For \code{transform},
    \code{by} names optional grouping columns: each expression in \code{...}
    is then evaluated within every group (e.g. \code{cumsum(x)} or
    \code{x - mean(x)} computed per group) instead of across the whole
    table, so the result still has one row per input row. Use
    \code{\link{summaries}} for one row per group instead.}
  \item{drop, na.last, .keep, decreasing}{Control flags.}
  \item{...}{Additional expressions or arguments passed through to helpers.}
  \item{direction}{Additional operation-specific controls.}
}
\value{
Most functions return a data.table.
}
\examples{
subset(mtcars, cyl == 6, select = c("mpg", "hp"))
transform(mtcars, power = hp / wt)
transform(mtcars, dev_mpg = mpg - mean(mpg), by = "cyl")
}
