\name{profile}
\alias{profile}
\title{Compact variable profile}
\usage{
profile(data, cols = NULL, top_n = 3)
}
\arguments{
\item{data}{A data frame or data table.}

\item{cols}{Optional character vector of columns to profile.}

\item{top_n}{Number of most frequent values to show for non-numeric columns.}
}
\value{
A tibble with one row per column.
}
\description{
Return a compact profile table for the variables in a data set.
}
