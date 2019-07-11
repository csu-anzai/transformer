#' Coerce to `data.frame`
#'
#' @name coerce-data.frame
#' @inheritParams base::as.data.frame
#'
#' @examples
#' ## sparseMatrix to data.frame ====
#' data(sparse, package = "acidtest")
#' stopifnot(is(sparse, "sparseMatrix"))
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
# > getMethod("as.data.frame", "GenomicRanges")
# > getMethod("as.data.frame", "IRanges")  # Inherits from `IPosRanges`.
# > getMethod("as.data.frame", "IPosRanges")
#
# See also:
# - https://github.com/Bioconductor/IRanges/issues/8
#
# Updated 2019-07-11.
#' @rdname coerce-data.frame
#' @export
as.data.frame.IPosRanges <-  # nolint
    function(
        x,
        row.names = NULL,
        optional = FALSE,
        ...
    ) {
        if (!(is.null(row.names) || is.character(row.names))) {
            stop("`row.names` must be NULL or a character vector.")
        }
        if (is.null(row.names)) {
            row.names <- names(x)
        }
        if (!is.null(names(x))) {
            names(x) <- NULL
        }
        mcols_df <- as.data.frame(mcols(x, use.names = FALSE))
        data.frame(
            start = start(x),
            end = end(x),
            width = width(x),
            mcols_df,
            row.names = row.names,
            check.rows = TRUE,
            check.names = FALSE,
            stringsAsFactors = FALSE
        )
    }

#' @rdname coerce-data.frame
#' @export
setMethod(
    f = "as.data.frame",
    signature = signature("IPosRanges"),
    definition = as.data.frame.IPosRanges
)



# Updated 2019-07-11.
as.data.frame.sparseMatrix <-  # nolint
    function(x, ...) {
        as.data.frame(as.matrix(x), ...)
    }

#' @rdname coerce-data.frame
#' @export
setMethod(
    f = "as.data.frame",
    signature = signature("sparseMatrix"),
    definition = as.data.frame.sparseMatrix
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
