#' Coerce to data table
#'
#' Coerce to `data.table`.
#'
#' @name coerce-data.table
#' @inheritParams data.table::as.data.table
#' @inheritParams params
#'
#' @details
#' Our defined methods attempt to improve on the defaults in the data.table
#' package to ensure that row names are not dropped by default, which is a poor
#' default for bioinformatics. This is accomplished by setting
#' `keep.rownames = "rowname"` by default instead of `keep.rownames = NULL`.
#' Note that we're manually defining the "`rowname`" column instead of using
#' `TRUE`, to match the conventions used in our `as_tibble()` methods.
#'
#' @section S3 `as.data.table()`:
#'
#' transformer extends [`as.data.table()`][data.table::as.data.table] method
#' support for these S4 classes:
#'
#' - `DataFrame`.
#' - `GenomicRanges`.
#'
#' @section S4 `as()`:
#'
#' Since `data.table` is a class that extends `data.frame`, we need to define an
#' S4 coercion method that allows us to use [`as()`][methods::as] to coerce an
#' object to a `data.table`.
#'
#' See `getClass("data.table")` for details.
#'
#' @seealso [data.table::as.data.table()].
#'
#' @examples
#' data(DataFrame, GRanges, IRanges, package = "acidtest")
#'
#' ## DataFrame to data.table ====
#' x <- as(DataFrame, "data.table")
#' x <- as.data.table(DataFrame)
#' print(x)
#'
#' ## GenomicRanges to data.table ====
#' x <- as(GRanges, "data.table")
#' x <- as.data.table(GRanges)
#' print(x)
#'
#' ## IRanges to data.table ====
#' x <- as(IRanges, "data.table")
#' x <- as.data.table(IRanges)
#' print(x)
NULL



#' @rdname coerce-data.table
#' @name as.data.table
#' @importFrom data.table as.data.table
#' @usage as.data.table(x, keep.rownames = FALSE, ...)
#' @export
NULL



## Note that we're matching `as_tibble()` convention here, using "rowname" as
## column for row names assignment. We also using similar internal assert checks
## here, allowing atomic and/or list columns only.

#' @rdname coerce-data.table
#' @export
## Updated 2019-07-19.
as.data.table.DataFrame <-  # nolint
    function(x, keep.rownames = TRUE, ...) {  # nolint
        x <- `.coerce,DataFrame,data.frame`(x)
        if (!hasRownames(x)) {
            keep.rownames <- FALSE  # nolint
        }
        as.data.table(x = x, keep.rownames = keep.rownames, ...)
    }



## The default handling from data.frame isn't clean, so add this.
## Default method will warn: `Arguments in '...' ignored`.

#' @rdname coerce-data.table
#' @export
## Updated 2019-07-19.
as.data.table.IPosRanges <-  # nolint
    function(x, keep.rownames = TRUE, ...) {  # nolint
        x <- as(x, "data.frame")
        if (!hasRownames(x)) {
            keep.rownames <- FALSE  # nolint
        }
        as.data.table(x = x, keep.rownames = keep.rownames, ...)
    }



#' @rdname coerce-data.table
#' @export
## Updated 2019-07-20.
as.data.table.GenomicRanges <- as.data.table.IPosRanges
