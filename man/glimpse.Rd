\name{glimpse}
\alias{glimpse}
\alias{dims}
\alias{types}
\alias{headtail}
\alias{describe}
\alias{missingness}
\alias{profile}
\alias{freq}
\alias{compare}
\title{Exploratory data analysis operations}
\description{
Compact inspection and exploratory summaries for tabular data.
}
\usage{
glimpse(data, width = getOption("width"))

dims(data)

types(data)

headtail(data, n = 3)

describe(data, cols = NULL, top_n = 3)

missingness(data, margin = c("column", "row"))

profile(data, cols = NULL, top_n = 3)

freq(data, column, by = NULL, prop = FALSE, sort = TRUE)

compare(x, y, by = NULL)
}
\arguments{
  \item{data, x, y}{A \code{data.frame} or \code{data.table}.}
  \item{cols, column, by}{Character vectors naming columns.}
  \item{n, top_n, width}{Display controls.}
  \item{margin}{Whether missingness is summarized by column or row.}
  \item{prop, sort}{Frequency-table controls.}
}
\value{
Most functions return a plain \code{data.frame}. \code{compare()} returns a
named list of comparison tables.
}
\examples{
dims(iris)
types(iris)
describe(iris)
missingness(airquality)
freq(iris, "Species", prop = TRUE)
}
