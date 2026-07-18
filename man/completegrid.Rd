\name{completegrid}
\alias{completegrid}
\title{Complete a table grid}
\usage{
completegrid(data, cols, fill = list())
}
\arguments{
\item{data}{A data frame or data table.}

\item{cols}{Character vector of columns to complete across.}

\item{fill}{Named list of values used to fill missing cells.}
}
\value{
A tibble containing the completed grid.
}
\description{
Create all combinations of selected columns and merge them back to the input.
}
