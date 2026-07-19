\name{rangemerge}
\alias{rangemerge}
\title{Merge two tables by a range condition}
\usage{
rangemerge(x, y, by, lower, upper, value)
}
\arguments{
\item{x, y}{Data frames or data tables to join.}

\item{by}{Character vector of exact-match join columns, present in both
\code{x} and \code{y}.}

\item{lower, upper}{Single columns in \code{x} describing the inclusive
range bounds to test against.}

\item{value}{Single column in \code{y} whose value is tested against each
matching \code{x} row's \verb{[lower, upper]} range.}
}
\value{
A data.table with one row per \code{x} row for each \code{y} row whose
\code{value} falls within \verb{[lower, upper]} (matching on \code{by}); an
\code{x} row with no matching \code{y} row appears once with \code{NA} for
\code{y}'s columns.
}
\description{
For each row of \code{x}, keep the row(s) of \code{y} whose \code{value}
column falls between \code{x}'s \code{lower} and \code{upper} bounds
(inclusive), within the exact-match \code{by} key.
}
\examples{
x <- data.frame(id = c(1, 1, 2), lower = c(0, 0, 0), upper = c(10, 10, 20))
y <- data.frame(id = c(1, 1, 2), val = c(5, 15, 5), label = c("a", "b", "c"))
rangemerge(x, y, by = "id", lower = "lower", upper = "upper", value = "val")
}
