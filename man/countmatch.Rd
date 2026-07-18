\name{countmatch}
\alias{countmatch}
\title{Count regex matches}
\usage{
countmatch(x, pattern, ignore_case = FALSE)
}
\arguments{
\item{x}{A character vector or factor.}

\item{pattern}{A regular expression.}

\item{ignore_case}{Whether case should be ignored.}
}
\value{
An integer vector giving the number of regex matches in each element.
}
\description{
Count how many times a regular expression matches each element of text.
}
