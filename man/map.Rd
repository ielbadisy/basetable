\name{map}
\alias{map}
\alias{pmap}
\alias{walk}
\alias{pwalk}
\title{Mapping helpers}
\description{
Small base-R-style mapping helpers with no external dependency. See
\code{\link{reduce}} for the reducing counterpart, documented on its own
page.
}
\usage{
map(.x, .f, ...)

pmap(.l, .f, ...)

walk(.x, .f, ...)

pwalk(.l, .f, ...)
}
\arguments{
  \item{.x}{A vector or list.}
  \item{.f}{A function or function name.}
  \item{.l}{A list of vectors or lists with a common length.}
  \item{...}{Additional arguments passed to \code{.f}.}
}
\value{
\code{map()} and \code{pmap()} return lists.
\code{walk()} and \code{pwalk()} return their input invisibly.
}
\examples{
map(1:3, function(x) x + 1)
pmap(list(a = 1:2, b = 10:11), function(a, b) a + b)
}
