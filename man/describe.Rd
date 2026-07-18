\name{describe}
\alias{describe}
\title{Column descriptions}
\usage{
describe(data, cols = NULL, top_n = 3)
}
\arguments{
\item{data}{A data frame or data table.}

\item{cols}{Optional character vector of columns to describe.}

\item{top_n}{Number of most frequent values to show for non-numeric columns.}
}
\value{
A tibble with one row per column.
}
\description{
Summarize a table with basic counts, missingness, distinct values, and
descriptive statistics.
}
