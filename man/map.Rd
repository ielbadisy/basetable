\name{map}
\alias{map}
\alias{traverse}
\alias{foldr}
\title{Functional helpers}
\description{
Small base-R-style functional helpers with no external dependency.
}
\usage{
map(.x, .f, ...)

traverse(.l, .f, ...)

foldr(.x, .f, .init = NULL, .accumulate = FALSE, .simplify = TRUE)
}
\arguments{
\item{.x}{A vector or list, used by \code{map()} and \code{foldr()}.}

\item{.f}{A function or function name.}

\item{...}{Additional arguments passed to \code{.f}.}

\item{.l}{A list of vectors or lists with a common length.}

\item{.init}{Optional initial value.}

\item{.accumulate}{Return intermediate accumulated values.}

\item{.simplify}{Simplify accumulated results when possible.}
}
\value{
\code{map()} and \code{traverse()} return lists.
\code{foldr()} returns the folded value, or accumulated values when
\code{.accumulate = TRUE}.
}
\examples{
map(1:3, function(x) x + 1)
traverse(list(a = 1:2, b = 10:11), function(a, b) a + b)
foldr(1:4, `+`)
}
