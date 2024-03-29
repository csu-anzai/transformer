% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deprecated.R
\name{defunct}
\alias{defunct}
\title{Defunct functions}
\value{
\code{.Defunct}.
}
\description{
Defunct functions
}
\examples{
a <- function(...) {
    .Defunct("b")
}

withCallingHandlers(
    expr = tryCatch(
        expr = a(1L),
        error = function(e) {
            print(e)
            invisible()
        }
    )
)
}
\seealso{
\link[base:Defunct]{Defunct}.
}
\keyword{internal}
