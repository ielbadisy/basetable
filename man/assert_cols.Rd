\name{assert_cols}
\alias{assert_cols}
\alias{assert_key}
\alias{common_names}
\alias{duplicated_keys}
\title{Validation and schema helpers}
\description{
Low-level helpers for validating columns and keys or comparing schemas.
}
\usage{
assert_cols(data, cols)

assert_key(data, by)

common_names(x, y)

duplicated_keys(data, by)
}
\arguments{
  \item{data, x, y}{A \code{data.frame} or \code{data.table}.}
  \item{cols, by}{Character vectors naming columns.}
}
\value{
\code{assert_cols()} and \code{assert_key()} return their input invisibly.
\code{common_names()} returns a character vector. \code{duplicated_keys()}
returns a data frame of duplicated key combinations.
}
\examples{
common_names(mtcars, iris)
assert_cols(mtcars, c("mpg", "hp"))
duplicated_keys(data.frame(id = c(1, 1, 2)), "id")
}
