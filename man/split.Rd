\name{split}
\alias{split}
\title{Split a table by groups}
\usage{
split(data, by, drop = FALSE, keep.by = TRUE)
}
\arguments{
\item{data}{A data frame or data table, or an atomic vector.}

\item{by}{For a data frame, a character vector of grouping columns. For an
atomic vector, a factor (or object coercible to one) the same length as
\code{data}, as in \code{base::split()}.}

\item{drop}{Whether to drop empty groups.}

\item{keep.by}{Whether to keep grouping columns in each piece. Ignored
when \code{data} is not a data frame.}
}
\value{
A named list of basetables (data frame input) or a named list of vectors
(atomic vector input, matching \code{base::split()}).
}
\description{
Split a table into pieces by one or more grouping columns. When \code{data}
is not a data frame, dispatches to \code{base::split()} so the classic
\code{split(vector, factor)} idiom keeps working with basetable attached.
}
