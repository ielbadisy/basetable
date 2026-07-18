\name{keepmissing}
\alias{keepmissing}
\title{Keep rows with missing values}
\usage{
keepmissing(data, cols = NULL, mode = c("any", "all"))
}
\arguments{
\item{data}{A data frame or data table.}

\item{cols}{Optional character vector of columns to inspect.}

\item{mode}{Whether any or all selected columns must be missing.}
}
\value{
A tibble containing the matching rows.
}
\description{
Return rows where selected columns are missing or blank.
}
