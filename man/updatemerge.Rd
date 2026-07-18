\name{updatemerge}
\alias{updatemerge}
\title{Update rows from another table}
\usage{
updatemerge(x, y, by, cols = NULL)
}
\arguments{
\item{x, y}{Data frames or data tables.}

\item{by}{Character vector of key columns.}

\item{cols}{Optional columns to update.}
}
\value{
A tibble with selected columns updated from \code{y}.
}
\description{
Update matching rows in \code{x} from \code{y}.
}
