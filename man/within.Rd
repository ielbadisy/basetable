\name{within}
\alias{within}
\title{Modify a table within an environment}
\usage{
within(data, expr)
}
\arguments{
\item{data}{A data frame or data table.}

\item{expr}{An expression evaluated in the data's environment.}
}
\value{
A tibble containing the modified table.
}
\description{
Evaluate expressions inside a table and return the modified result.
}
