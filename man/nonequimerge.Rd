\name{nonequimerge}
\alias{nonequimerge}
\title{Merge two tables with explicit arguments}
\usage{
nonequimerge(x, y, by, ...)
}
\arguments{
\item{x, y}{Data frames or data tables to join.}

\item{by}{Character vector of join columns.}

\item{...}{Additional arguments passed to \code{merge()}.}
}
\value{
A tibble containing the merged rows.
}
\description{
Merge two tables using the package merge helper.
}
