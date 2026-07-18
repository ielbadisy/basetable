\name{rangemerge}
\alias{rangemerge}
\title{Merge a table by range columns}
\usage{
rangemerge(x, y, by, lower, upper)
}
\arguments{
\item{x, y}{Data frames or data tables to join.}

\item{by}{Character vector of join columns.}

\item{lower, upper}{Single columns in \code{x} describing the range bounds.}
}
\value{
A tibble containing the merged rows.
}
\description{
Merge two tables using explicit range columns.
}
