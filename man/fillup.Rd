\name{fillup}
\alias{fillup}
\title{Fill missing values upward}
\usage{
fillup(data, cols, by = NULL)
}
\arguments{
\item{data}{A data frame or data table.}

\item{cols}{Character vector of columns to fill.}

\item{by}{Optional grouping columns.}
}
\value{
A tibble with missing values filled upward.
}
\description{
Fill missing values in selected columns using the next observed value.
}
