\name{textdist}
\alias{textdist}
\title{Compute text distances}
\usage{
textdist(x, y = NULL, ignore_case = FALSE)
}
\arguments{
\item{x}{A character vector or factor.}

\item{y}{Optional second character vector or factor. If omitted, distances are computed within \code{x}.}

\item{ignore_case}{Whether case should be ignored.}
}
\value{
A distance matrix from \code{utils::adist()}.
}
\description{
Compute edit distances between text vectors.
}
