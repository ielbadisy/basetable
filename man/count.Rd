\name{count}
\alias{count}
\title{Count rows by group}
\usage{
count(data, by, sort = TRUE, name = "n")
}
\arguments{
\item{data}{A data frame or data table.}

\item{by}{Character vector of grouping columns.}

\item{sort}{Whether to sort by descending counts.}

\item{name}{Name of the count column.}
}
\value{
A tibble with one row per group.
}
\description{
Count rows in a table by one or more grouping columns.
}
