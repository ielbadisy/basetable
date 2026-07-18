\name{distinct}
\alias{distinct}
\title{Distinct rows}
\usage{
distinct(data, cols = NULL, .keep_all = FALSE)
}
\arguments{
\item{data}{A data frame or data table.}

\item{cols}{Optional character vector of columns used to determine uniqueness.}

\item{.keep_all}{Whether to keep all columns when `cols` is supplied.}
}
\value{
A tibble containing distinct rows.
}
\description{
Return unique rows, optionally using only selected columns to define
uniqueness.
}
