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
