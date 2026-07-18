\name{split}
\alias{split}
\title{Split a table by groups}
\usage{
split(data, by, drop = FALSE, keep.by = FALSE)
}
\arguments{
\item{data}{A data frame or data table.}

\item{by}{Character vector of grouping columns.}

\item{drop}{Whether to drop empty groups.}

\item{keep.by}{Whether to keep grouping columns in each piece.}
}
\value{
A named list of tibbles.
}
\description{
Split a table into pieces by one or more grouping columns.
}
