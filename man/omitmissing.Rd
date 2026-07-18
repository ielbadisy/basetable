\name{omitmissing}
\alias{omitmissing}
\title{Drop rows with missing values}
\usage{
omitmissing(data, cols = NULL, mode = c("any", "all"))
}
\arguments{
\item{data}{A data frame or data table.}

\item{cols}{Optional character vector of columns to inspect.}

\item{mode}{Whether any or all selected columns must be missing before a row is dropped.}
}
\value{
A tibble without the omitted rows.
}
\description{
Drop rows where selected columns are missing or blank.
}
