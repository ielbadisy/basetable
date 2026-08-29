\name{freq}
\alias{freq}
\title{Frequency table}
\usage{
freq(data, column, by = NULL, prop = FALSE, sort = TRUE, ...)
}
\arguments{
\item{data}{A data frame, or a single path to a delimited text file. In file
mode the second argument is taken as the grouping column(s).}

\item{column}{Name of the column to tabulate.}

\item{by}{Optional grouping column or columns.}

\item{prop}{Whether to include proportions.}

\item{sort}{Whether to sort rows by descending frequency.}

\item{...}{In file mode, reader options such as \code{where}, \code{delim}.}
}
\value{
A basetable containing frequencies, and proportions when requested.
}
\description{
Count the occurrences of values in a column, optionally within groups.
}
