#' Coerce to data frame
#'
#' Coerce to `data.frame`.
#'
#' @name coerce-data.frame
#' @inheritParams base::as.data.frame
#' @inheritParams params
#'
#' @examples
#' data(sparse, package = "acidtest")
#'
#' ## sparseMatrix to data.frame ====
#' x <- as(sparse, "data.frame")
#' head(x)
NULL



# S3(ish) ======================================================================
#' @rdname coerce-data.frame
#' @name as.data.frame
#' @importFrom BiocGenerics as.data.frame
#' @usage as.data.frame(x, row.names = NULL, optional = FALSE, ...)
#' @export
NULL



# Default coercion of IRanges to data.frame currently strips metadata in
# `mcols()`. However, GenomicRanges preserves this information, so we're adding
# a coerce method here to improve consistency.
#
# Relevant methods:
# > getMethod(
# >     f = "as.data.frame",
# >     signature = "GenomicRanges",
# >     where = asNamespace("GenomicRanges")
# > )
# IRanges inherits from `IPosRanges`.
# > getMethod(
# >     f = "as.data.frame",
# >     signature = "IPosRanges",
# >     where = asNamespace("IRanges")
# )
#
# See also:
# - https://github.com/Bioconductor/IRanges/issues/8
#
# Updated 2019-07-11.
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



# Updated 2019-07-11.
`as.data.frame,sparseMatrix` <-  # nolint
    function(x, ...) {
        as.data.frame(as.matrix(x), ...)
    }



#' @rdname coerce-data.frame
#' @export
setMethod(
    f = "as.data.frame",
    signature = signature("sparseMatrix"),
    definition = `as.data.frame,sparseMatrix`
)



# S4 ===========================================================================
# Updated 2019-07-19.
`coerce,sparseMatrix,data.frame` <-  # nolint
    function(from) {
        as.data.frame(from)
    }



#' @rdname coerce-data.frame
#' @name coerce,sparseMatrix,data.frame-method
setAs(
    from = "sparseMatrix",
    to = "data.frame",
    def = `coerce,sparseMatrix,data.frame`
)



# Updated 2019-07-19.
`coerce,IPosRanges,data.frame` <-  # nolint
    function(from) {
        as.data.frame(from)
    }



#' @rdname coerce-data.frame
#' @name coerce,IPosRanges,data.frame-method
setAs(
    from = "IPosRanges",
    to = "data.frame",
    def = `coerce,IPosRanges,data.frame`
)
