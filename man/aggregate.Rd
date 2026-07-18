\name{aggregate}
\alias{aggregate}
\title{Aggregate values by group}
\usage{
aggregate(data, by, value = NULL, fun, ..., na.rm = FALSE, sort = TRUE)
}
\arguments{
\item{data}{A data frame or data table.}

\item{by}{Character vector of grouping columns.}

\item{value}{Optional character vector of value columns to summarize.}

\item{fun}{Summary function applied to each value column.}

\item{...}{Additional arguments passed to \code{fun}.}

\item{na.rm}{Whether to remove missing values before summarizing.}

\item{sort}{Whether to sort output rows by group.}
}
\value{
A data.table containing one row per group.
}
\description{
Compute grouped summaries for one or more value columns.
}
