#' Coerce to `data.frame`
#'
#' @name coerce-data.frame
#' @inheritParams base::as.data.frame
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
#' @rdname coerce-data.frame
#' @export
setMethod(
    f = "as.data.frame",
    signature = signature("IPosRanges"),
    definition = function(
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
)



# Updated 2019-07-11.
#' @rdname coerce-data.frame
#' @export
setMethod(
    f = "as.data.frame",
    signature = signature("sparseMatrix"),
    definition = function(x, ...) {
        as.data.frame(as.matrix(x), ...)
    }
)



# S4 ===========================================================================
# Updated 2019-07-11.
#' @rdname coerce-data.frame
#' @name coerce,sparseMatrix,data.frame-method
setAs(
    from = "sparseMatrix",
    to = "data.frame",
    def = function(from) {
        as.data.frame(from)
    }
)



# Updated 2019-07-11.
#' @rdname coerce-data.frame
#' @name coerce,IPosRanges,data.frame-method
setAs(
    from = "IPosRanges",
    to = "data.frame",
    def = function(from) {
        as.data.frame(from)
    }
)
