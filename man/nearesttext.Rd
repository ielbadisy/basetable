\name{nearesttext}
\alias{nearesttext}
\title{Find the nearest candidate text}
\usage{
nearesttext(x, candidates, ignore_case = FALSE)
}
\arguments{
\item{x}{A character vector or factor.}

\item{candidates}{Candidate strings to compare against.}

\item{ignore_case}{Whether case should be ignored.}
}
\value{
A character vector containing the nearest candidate for each input.
}
\description{
Return the nearest candidate string for each element of text.
}
