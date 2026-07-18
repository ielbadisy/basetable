\name{antimerge}
\alias{antimerge}
\title{Anti-join two tables}
\usage{
antimerge(x, y, by)
}
\arguments{
\item{x, y}{Data frames or data tables to compare.}

\item{by}{Character vector of join columns.}
}
\value{
A tibble containing rows from \code{x} without matches in \code{y}.
}
\description{
Keep rows in \code{x} whose keys do not exist in \code{y}.
}
