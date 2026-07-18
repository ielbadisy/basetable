\name{missingindicator}
\alias{missingindicator}
\title{Add missingness indicators}
\usage{
missingindicator(data, cols = NULL, prefix = "missing_")
}
\arguments{
\item{data}{A data frame or data table.}

\item{cols}{Optional character vector of columns to inspect.}

\item{prefix}{Prefix used for the added indicator columns.}
}
\value{
A tibble with logical missingness indicator columns appended.
}
\description{
Append logical indicators showing whether selected columns are missing or blank.
}
