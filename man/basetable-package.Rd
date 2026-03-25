\name{basetable-package}
\alias{basetable-package}
\alias{basetable}
\title{Base-Faithful Tabular Data Tools}
\description{
The \pkg{basetable} package provides a compact set of base-style data
manipulation and exploratory data analysis helpers backed primarily by
\pkg{data.table}.
}
\details{
Core design rules:
\itemize{
\item exported data verbs take the main data object as their first argument
\item grouping is explicit with \code{by =}
\item outputs are plain data frames or lists in most cases
\item interfaces are intended to work naturally in nested calls and native
\code{|>} pipelines
}
}
\docType{package}
