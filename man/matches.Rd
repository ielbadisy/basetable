\name{matches}
\alias{matches}
\title{Detect regex matches}
\usage{
matches(x, pattern, ignore_case = FALSE)
}
\arguments{
\item{x}{A character vector or factor.}

\item{pattern}{A regular expression.}

\item{ignore_case}{Whether case should be ignored.}
}
\value{
A logical vector indicating whether each element matches the pattern.
}
\description{
Detect whether text matches a regular expression.
}
