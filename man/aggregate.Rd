\name{aggregate}
\alias{aggregate}
\title{Aggregate values by group}
\usage{
aggregate(data, by, value = NULL, fun, ..., na.rm = FALSE, sort = TRUE)
}
\arguments{
\item{data}{A data frame, or a single path to a delimited text file for the
fused one-pass file mode.}

\item{by}{Character vector of grouping columns.}

\item{value}{Optional character vector of value columns to summarize.}

\item{fun}{Summary function applied to each value column. In file mode this is
one or more of \code{"sum"}, \code{"mean"}, \code{"var"}, \code{"sd"},
\code{"min"}, \code{"max"}, \code{"n"} and defaults to \code{"sum"}.}

\item{...}{Additional arguments passed to \code{fun}; in file mode, reader
options such as \code{where}, \code{delim}, \code{n_threads}.}

\item{na.rm}{Whether to remove missing values before summarizing (defaults to
\code{TRUE} in file mode).}

\item{sort}{Whether to sort output rows by group.}
}
\value{
A basetable containing one row per group.
}
\description{
Compute grouped summaries for one or more value columns. When \code{data} is a
single file path, the file is scanned once and only the grouping and value
fields are extracted, without materialising the other columns.
}
