#' @rdname coerce-data.frame
#' @name as.data.frame
#' @importFrom BiocGenerics as.data.frame
#' @usage as.data.frame(x, row.names = NULL, optional = FALSE, ...)
#' @export
NULL



## Default coercion of IPosRanges (i.e. IRanges) to data.frame currently strips
## metadata in `mcols()`. However, GenomicRanges preserves this information, so
## we're adding a tweaked coercion method here to improve consistency.
##
## Relevant methods:
## > getMethod(
## >     f = "as.data.frame",
## >     signature = "GenomicRanges",
## >     where = asNamespace("GenomicRanges")
## > )
## IRanges inherits from `IPosRanges`.
## > getMethod(
## >     f = "as.data.frame",
## >     signature = "IPosRanges",
## >     where = asNamespace("IRanges")
## > )
##
## See also:
## - https://github.com/Bioconductor/IRanges/issues/8
##
## Updated 2019-07-20.
`as.data.frame,IPosRanges` <-  # nolint
    function(
        x,
        row.names = NULL,
        optional = FALSE,
        ...
    ) {
        if (missing(row.names)) {
            row.names <- names(x)
        }
        if (!is.null(names(x))) {
            names(x) <- NULL
        }
        args <- list(
            start = start(x),
            end = end(x),
            width = width(x),
            row.names = row.names,
            check.rows = TRUE,
            check.names = FALSE,
            stringsAsFactors = FALSE
        )
        mcols <- mcols(x, use.names = FALSE)
        if (!is.null(mcols)) {
            args[["mcols"]] <- as.data.frame(mcols)
        }
        do.call(what = data.frame, args = args)
    }



#' @rdname coerce-data.frame
#' @export
setMethod(
    f = "as.data.frame",
    signature = signature("IPosRanges"),
    definition = `as.data.frame,IPosRanges`
)



## Updated 2019-07-20.
`as.data.frame,Matrix` <-  # nolint
    function(x, ...) {
        as.data.frame(as.matrix(x), ...)
    }



#' @rdname coerce-data.frame
#' @export
setMethod(
    f = "as.data.frame",
    signature = signature("Matrix"),
    definition = `as.data.frame,Matrix`
)
