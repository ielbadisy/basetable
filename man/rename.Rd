\name{rename}
\alias{rename}
\title{Rename columns}
\usage{
rename(data, ...)
}
\arguments{
\item{data}{A data frame or data table.}

\item{...}{Named rename expressions of the form `new_name = old_name`.}
}
\value{
A tibble with renamed columns.
}
\description{
Rename one or more columns using `new = old` pairs.
}
