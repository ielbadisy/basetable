\name{unstack}
\alias{unstack}
\title{Unstack a table}
\usage{
unstack(data, form, ...)
}
\arguments{
\item{data}{A data frame or data table.}

\item{form}{A formula or equivalent description of the reshape.}

\item{...}{Additional arguments passed to the underlying method.}
}
\value{
A tibble reshaped into wide format.
}
\description{
Reshape a stacked table back into wide form.
}
