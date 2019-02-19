#' Coerce to `data.table`
#'
#' @name coerce-data.table
#'
#' @section S3 `as.data.table()`:
#'
#' transformer extends [`as.data.table()`][data.table::as.data.table] method
#' support for these S4 classes:
#'
#' - `DataFrame`.
#' - `GRanges`.
#'
#' @section S4 `as()`:
#'
#' Since `data.table` is a class that extends `data.frame`, we need to define an
#' S4 coercion method that allows us to use [`as()`][methods::as] to coerce an
#' object to a `data.table`.
#'
#' See `getClass("data.table")` for details.
#'
#' @examples
#' load(system.file("extdata", "rse.rda", package = "transformer"))
#'
#' df <- SummarizedExperiment::colData(rse)
#' gr <- SummarizedExperiment::rowRanges(rse)
#'
#' ## DataFrame to data.table ====
#' x <- as(df, "data.table")
#' x <- as.data.table(df)
#' print(x)
#'
#' ## GRanges to data.table ====
#' x <- as(gr, "data.table")
#' x <- as.data.table(gr)
#' print(x)
NULL



# S3 ===========================================================================
#' @importFrom data.table as.data.table
#' @aliases NULL
#' @export
data.table::as.data.table



# Note that we're matching `as_tibble()` convention here, using "rowname" as
# column for row names assignment. We also using similar internal assert checks
# here, allowing atomic and/or list columns only.

#' @method as.data.table DataFrame
#' @export
as.data.table.DataFrame <-  # nolint
    function(x, keep.rownames = "rowname", ...) {  # nolint
        x <- .coerceDataFrame(x)
        if (!hasRownames(x)) {
            keep.rownames <- FALSE  # nolint
        }
        as.data.table(x = x, keep.rownames = keep.rownames, ...)
    }



# The default handling from data.frame isn't clean, so add this.
# Default method will warn: `Arguments in '...' ignored`.
#' @method as.data.table GRanges
#' @export
as.data.table.GRanges <-  # nolint
    function(x, keep.rownames = "rowname", ...) {  # nolint
        names <- names(x)
        x <- as(x, "data.frame")
        rownames(x) <- names
        if (!hasRownames(x)) {
            keep.rownames <- FALSE  # nolint
        }
        as.data.table(x = x, keep.rownames = keep.rownames, ...)
    }



# S4 ===========================================================================
#' @rdname coerce-data.table
#' @name coerce,data.frame,data.table-method
setAs(
    from = "data.frame",
    to = "data.table",
    def = function(from) {
        as.data.table(from)
    }
)



#' @rdname coerce-data.table
#' @name coerce,DataFrame,data.table-method
setAs(
    from = "DataFrame",
    to = "data.table",
    def = function(from) {
        as.data.table(from)
    }
)



#' @rdname coerce-data.table
#' @name coerce,GRanges,data.table-method
setAs(
    from = "GRanges",
    to = "data.table",
    def = function(from) {
        as.data.table(from)
    }
)
