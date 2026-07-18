\name{overlapmerge}
\alias{overlapmerge}
\title{Merge tables on overlapping intervals}
\usage{
overlapmerge(x, y, startx, endx, starty, endy, by = NULL)
}
\arguments{
\item{x, y}{Data frames or data tables to join.}

\item{startx, endx}{Interval bounds in \code{x}.}

\item{starty, endy}{Interval bounds in \code{y}.}

\item{by}{Optional character vector of exact-match grouping columns.}
}
\value{
A tibble containing the overlap matches.
}
\description{
Join tables by overlapping interval ranges.
}
