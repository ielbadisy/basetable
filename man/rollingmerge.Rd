\name{rollingmerge}
\alias{rollingmerge}
\title{Rolling join two tables}
\usage{
rollingmerge(x, y, by, direction = c("backward", "forward", "nearest"),
  tolerance = Inf)
}
\arguments{
\item{x, y}{Data frames or data tables to join.}

\item{by}{Character vector of join columns. The last column is treated as the
rolling key.}

\item{direction}{Rolling direction: backward, forward, or nearest.}

\item{tolerance}{Maximum distance allowed for rolling matches.}
}
\value{
A tibble with rows from \code{x} joined to nearest matches in \code{y}.
}
\description{
Join tables using a rolling match on the last key column.
}
