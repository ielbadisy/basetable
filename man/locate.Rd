\name{locate}
\alias{locate}
\title{Locate the first regex match}
\usage{
locate(x, pattern, ignore_case = FALSE)
}
\arguments{
\item{x}{A character vector or factor.}

\item{pattern}{A regular expression.}

\item{ignore_case}{Whether case should be ignored.}
}
\value{
An integer vector giving the starting position of the first match, or \code{NA} if no match is found.
}
\description{
Return the position of the first regular-expression match in each element.
}
