\name{count}
\alias{count}
\title{Count rows by group}
\usage{
count(data, by, sort = TRUE, name = "n", ...)
}
\arguments{
\item{data}{A data frame, or a single path to a delimited text file for the
fused one-pass file mode.}

\item{by}{Character vector of grouping columns.}

\item{sort}{Whether to sort by descending counts.}

\item{name}{Name of the count column.}

\item{...}{In file mode, reader options such as \code{where}, \code{delim}.}
}
\value{
A basetable with one row per group.
}
\description{
Count rows in a table by one or more grouping columns.
}
