\name{nonequimerge}
\alias{nonequimerge}
\title{Merge two tables using non-equi (inequality) conditions}
\usage{
nonequimerge(x, y, by, ...)
}
\arguments{
\item{x, y}{Data frames or data tables to join.}

\item{by}{Character vector of join conditions. A plain column name (present
in both \code{x} and \code{y}) is matched exactly; an entry containing a
comparison operator, written as \code{"xcol<op>ycol"} (e.g.
\code{"date>=start_date"}, \code{"date<=end_date"}), is matched as an
inequality between \code{x}'s \code{xcol} and \code{y}'s \code{ycol}. This
follows \code{data.table}'s own \code{on=} condition syntax.}

\item{...}{Additional arguments passed to \code{data.table}'s \code{[} join
(e.g. \code{mult}, \code{roll}).}
}
\value{
A data.table with one row per matching \code{x}/\code{y} pair (an inner
join: rows of \code{x} with no matching \code{y} row are dropped).
}
\description{
Unlike a plain equi-\code{\link{merge}}, \code{nonequimerge()} supports
inequality conditions between columns of \code{x} and \code{y} (for example,
matching each \code{x} row to every \code{y} row whose date range contains
\code{x}'s date), in addition to any exact-match columns in \code{by}.
}
\examples{
x <- data.frame(id = 1, date = as.Date("2024-01-15"))
y <- data.frame(
  id = 1,
  start_date = as.Date(c("2024-01-01", "2024-02-01")),
  end_date = as.Date(c("2024-01-31", "2024-02-28")),
  period = c("Jan", "Feb")
)
nonequimerge(x, y, by = c("id", "date>=start_date", "date<=end_date"))
}
