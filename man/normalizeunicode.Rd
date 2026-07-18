\name{normalizeunicode}
\alias{normalizeunicode}
\title{Normalize Unicode text}
\usage{
normalizeunicode(x, form = c("NFC", "NFD", "NFKC", "NFKD"))
}
\arguments{
\item{x}{A character vector or factor.}

\item{form}{Unicode normalization form.}
}
\value{
A character vector normalized to the requested Unicode form.
}
\description{
Normalize Unicode text using the stringi package.
}
