\name{merge}
\alias{merge}
\title{Merge two tables}
\usage{
merge(x, y, by = NULL, all = FALSE, all.x = all, all.y = all,
  sort = FALSE, suffixes = c(".x", ".y"))
}
\arguments{
\item{x, y}{Data frames or data tables to merge.}

\item{by}{Optional character vector of join columns.}

\item{all, all.x, all.y}{Controls for keeping unmatched rows.}

\item{sort}{Whether to sort the result.}

\item{suffixes}{Suffixes used for overlapping column names.}
}
\value{
A tibble containing the merged rows.
}
\description{
Merge two tables using shared keys.
}
