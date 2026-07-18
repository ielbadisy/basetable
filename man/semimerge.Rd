\name{semimerge}
\alias{semimerge}
\title{Semi-join two tables}
\usage{
semimerge(x, y, by)
}
\arguments{
\item{x, y}{Data frames or data tables to compare.}

\item{by}{Character vector of join columns.}
}
\value{
A tibble containing rows from \code{x} with matches in \code{y}.
}
\description{
Keep rows in \code{x} whose keys exist in \code{y}.
}
