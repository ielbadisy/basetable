\name{stack}
\alias{stack}
\title{Stack columns}
\usage{
stack(data, select = NULL, drop = FALSE)
}
\arguments{
\item{data}{A data frame or data table.}

\item{select}{Optional character vector of columns to stack.}

\item{drop}{Whether to drop rows with missing values.}
}
\value{
A tibble with a stacked value column.
}
\description{
Convert selected columns into long format with an indicator column.
}
