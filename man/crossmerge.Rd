\name{crossmerge}
\alias{crossmerge}
\title{Cartesian join two tables}
\usage{
crossmerge(x, y)
}
\arguments{
\item{x, y}{Data frames or data tables to combine.}
}
\value{
A tibble containing the Cartesian product of the rows.
}
\description{
Return every combination of rows from \code{x} and \code{y}.
}
