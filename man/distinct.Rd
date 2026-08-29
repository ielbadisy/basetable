\name{distinct}
\alias{distinct}
\title{Distinct rows, in memory or straight off a delimited file}
\usage{
distinct(data, cols = NULL, ...)
}
\arguments{
\item{data}{A data frame, or a single path to a delimited text file.}

\item{cols}{Columns whose distinct values (or combinations) to return.
\code{NULL} (the default) uses every column; a file requires \code{cols}.}

\item{...}{For the in-memory form, passed to \code{\link{uniquerows}} (e.g.
\code{.keep_all}); for the file form, reader options (\code{where},
\code{delim}, \code{n_threads}, ...).}
}
\value{
A basetable.
}
\description{
With a data frame, \code{distinct()} is \code{\link{uniquerows}}. With a
single file path, it is a fused one-pass scan returning the distinct
combinations of \code{cols} without materialising the file.
}
