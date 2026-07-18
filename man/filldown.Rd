\name{filldown}
\alias{filldown}
\title{Fill missing values downward}
\usage{
filldown(data, cols, by = NULL)
}
\arguments{
\item{data}{A data frame or data table.}

\item{cols}{Character vector of columns to fill.}

\item{by}{Optional grouping columns.}
}
\value{
A tibble with missing values filled downward.
}
\description{
Fill missing values in selected columns using the last observed value.
}
