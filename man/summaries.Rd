\name{summaries}
\alias{summaries}
\title{Named grouped summaries}
\usage{
summaries(data, by = NULL, ...)
}
\arguments{
\item{data}{A data frame or data table.}

\item{by}{Optional grouping columns.}

\item{...}{Named summary expressions.}
}
\value{
A tibble containing the requested summaries.
}
\description{
Compute multiple named summary expressions, optionally by group.
}
