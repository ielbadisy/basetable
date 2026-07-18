\name{mutate}
\alias{mutate}
\title{Mutate columns}
\usage{
mutate(data, ..., .keep = TRUE)
}
\arguments{
\item{data}{A data frame or data table.}

\item{...}{Named expressions that create or replace columns.}

\item{.keep}{Whether to keep existing columns.}
}
\value{
A tibble with the mutated columns.
}
\description{
Add or replace columns using expressions evaluated in the data.
}
