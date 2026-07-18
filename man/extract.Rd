\name{extract}
\alias{extract}
\title{Extract regex capture groups}
\usage{
extract(x, pattern, group = 1L, ignore_case = FALSE)
}
\arguments{
\item{x}{A character vector or factor.}

\item{pattern}{A regular expression with capture groups.}

\item{group}{Capture-group number to return, with 0 for the full match.}

\item{ignore_case}{Whether case should be ignored.}
}
\value{
A character vector containing the selected capture-group values.
}
\description{
Extract a capture group from the first regular-expression match in each element.
}
