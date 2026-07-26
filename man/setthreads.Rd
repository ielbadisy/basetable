\name{setthreads}
\alias{setthreads}
\title{Set data.table thread count for the session}
\usage{
setthreads(threads = NULL, restore_after_fork = NULL, percent = NULL, throttle = NULL)
}
\arguments{
\item{threads}{Integer thread count, or \code{NULL} to reread environment settings.}

\item{restore_after_fork}{Whether to restore multithreading after a fork.}

\item{percent}{Percentage of detected logical CPUs to use.}

\item{throttle}{Passed through to \code{data.table::setDTthreads()}.}
}
\value{
The previous thread count.
}
\description{
Wrapper around \code{data.table::setDTthreads()} that makes thread control
available from \code{basetable} without exposing it on every verb.
}
