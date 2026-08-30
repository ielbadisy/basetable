\name{subset}
\alias{subset}
\alias{transform}
\alias{reshape}
\title{Core data manipulation operations}
\description{
Base-faithful table manipulation helpers built around explicit arguments,
returning a \code{basetable}. See \code{\link{aggregate}}, \code{\link{count}},
\code{\link{drop}}, \code{\link{merge}}, \code{\link{move}},
\code{\link{orderrows}}, \code{\link{pick}}, \code{\link{renamecols}},
\code{\link{split}}, \code{\link{stack}}, \code{\link{summaries}},
\code{\link{uniquerows}}, \code{\link{unstack}}, and \code{\link{within}}
for the remaining core verbs, each documented on its own page.
}
\usage{
subset(data, subset = NULL, select = NULL, drop = FALSE, by = NULL)

transform(data, ..., .keep = TRUE, by = NULL)

reshape(data, ..., direction)
}
\arguments{
  \item{data}{A \code{data.frame}.}
  \item{subset}{A base-style expression evaluated against columns.}
  \item{select, by}{Character vectors naming columns. For \code{subset},
    \code{by} names optional grouping columns: the \code{subset} expression
    is then evaluated within every group, so aggregate references in it
    (\code{mean(x)}, \code{max(x)}, ...) are per group; kept rows are
    returned in their original order. For \code{transform}, \code{by} names
    optional grouping columns and each expression in \code{...} is evaluated
    within every group (e.g. \code{cumsum(x)} or \code{x - mean(x)} computed
    per group), so the result still has one row per input row. Use
    \code{\link{summaries}} for one row per group instead.}
  \item{drop, .keep}{Control flags.}
  \item{...}{Additional expressions or arguments passed through to helpers.}
  \item{direction}{Additional operation-specific controls.}
}
\value{
Most functions return a basetable.
}
\examples{
subset(mtcars, cyl == 6, select = c("mpg", "hp"))
subset(mtcars, mpg > mean(mpg), by = "cyl")
transform(mtcars, power = hp / wt)
transform(mtcars, dev_mpg = mpg - mean(mpg), by = "cyl")
}
