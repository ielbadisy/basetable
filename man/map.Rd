\name{map}
\alias{map}
\alias{map_lgl}
\alias{map_int}
\alias{map_dbl}
\alias{map_chr}
\alias{walk}
\title{Functionals-backed map helpers}
\description{
Small exported wrappers around the \pkg{functionals} mapping helpers. See
\code{\link{reduce}} for the reducing counterpart, documented on its own
page.
}
\usage{
map(.x, .f, ..., ncores = NULL, pb = FALSE)

map_lgl(.x, .f, ..., ncores = NULL, pb = FALSE)

map_int(.x, .f, ..., ncores = NULL, pb = FALSE)

map_dbl(.x, .f, ..., ncores = NULL, pb = FALSE)

map_chr(.x, .f, ..., ncores = NULL, pb = FALSE)

walk(.x, .f, ..., ncores = NULL, pb = FALSE)
}
\arguments{
  \item{.x}{A vector or list.}
  \item{.f}{A function or function name.}
  \item{...}{Additional arguments passed to \code{.f}.}
  \item{ncores, pb}{Parallelism and progress controls passed to \pkg{functionals}.}
}
\value{
\code{map()} returns a list. Typed map variants return atomic vectors.
\code{walk()} returns \code{.x} invisibly.
}
\examples{
map_dbl(1:3, function(x) x + 0.5)
map_chr(1:3, as.character)
}
