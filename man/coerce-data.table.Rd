% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as.data.table-methods.R,
%   R/coerce-data.table-methods.R
\name{as.data.table}
\alias{as.data.table}
\alias{as.data.table.DataFrame}
\alias{as.data.table.IPosRanges}
\alias{as.data.table.GenomicRanges}
\alias{coerce-data.table}
\alias{coerce,data.frame,data.table-method}
\alias{coerce,DataFrame,data.table-method}
\alias{coerce,IPosRanges,data.table-method}
\alias{coerce,GenomicRanges,data.table-method}
\title{Coerce to data table}
\usage{
as.data.table(x, keep.rownames = FALSE, ...)

\method{as.data.table}{DataFrame}(x, keep.rownames = TRUE, ...)

\method{as.data.table}{IPosRanges}(x, keep.rownames = TRUE, ...)

\method{as.data.table}{GenomicRanges}(x, keep.rownames = TRUE, ...)
}
\arguments{
\item{x}{An R object.}

\item{keep.rownames}{Default is \code{FALSE}. If \code{TRUE}, adds the input object's names as a separate column named \code{"rn"}. \code{keep.rownames = "id"} names the column \code{"id"} instead.}

\item{...}{Additional arguments to be passed to or from other methods.}
}
\value{
\code{data.table}.
}
\description{
Coerce to \code{data.table}.
}
\details{
Our defined methods attempt to improve on the defaults in the data.table
package to ensure that row names are not dropped by default, which is a poor
default for bioinformatics. This is accomplished by setting
\code{keep.rownames = "rowname"} by default instead of \code{keep.rownames = NULL}.
Note that we're manually defining the \code{"rowname"} column instead of using
\code{TRUE}, to match the conventions used in our \code{as_tibble()} methods.
}
\section{S3 \code{as.data.table()}}{


transformer extends \code{\link[data.table:as.data.table]{as.data.table()}} method
support for these S4 classes:
\itemize{
\item \code{DataFrame}.
\item \code{GenomicRanges}.
}
}

\section{S4 \code{as()}}{


Since \code{data.table} is a class that extends \code{data.frame}, we need to define an
S4 coercion method that allows us to use \code{\link[methods:as]{as()}} to coerce an
object to a \code{data.table}.

See \code{getClass("data.table")} for details.
}

\examples{
data(DataFrame, GRanges, IRanges, package = "acidtest")

## DataFrame to data.table ====
x <- as(DataFrame, "data.table")
x <- as.data.table(DataFrame)
print(x)

## GenomicRanges to data.table ====
x <- as(GRanges, "data.table")
x <- as.data.table(GRanges)
print(x)

## IRanges to data.table ====
x <- as(IRanges, "data.table")
x <- as.data.table(IRanges)
print(x)
}
\seealso{
\code{\link[data.table:as.data.table]{data.table::as.data.table()}}.
}
