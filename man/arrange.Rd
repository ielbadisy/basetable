\name{arrange}
\alias{arrange}
\title{Sort rows}
\usage{
arrange(data, by, decreasing = FALSE, na.last = TRUE)
}
\arguments{
\item{data}{A data frame or data table.}

\item{by}{Character vector of sort columns.}

\item{decreasing}{Whether to sort in descending order.}

\item{na.last}{Whether missing values sort last.}
}
\value{
A tibble with rows sorted by the requested columns.
}
\description{
Sort rows by one or more columns.
}
