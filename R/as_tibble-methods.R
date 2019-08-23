#' @rdname coerce-tbl_df
#' @name as_tibble
#' @importFrom tibble as_tibble
#' @export
NULL



rownames <- quote(pkgconfig::get_config("tibble::rownames", "rowname"))



#' @rdname coerce-tbl_df
#' @export
## Updated 2019-07-19.
as_tibble.DataFrame <-  # nolint
    function(x, ..., rownames) {
        x <- `.coerce,DataFrame,data.frame`(x)
        if (!hasRownames(x)) {
            rownames <- NULL
        }
        as_tibble(x = x, ..., rownames = rownames)
    }

formals(as_tibble.DataFrame)[["rownames"]] <- rownames



#' @rdname coerce-tbl_df
#' @export
## Updated 2019-07-20.
as_tibble.IPosRanges <-  # nolint
    function(x, ..., rownames) {
        x <- as(x, "data.frame")
        if (!hasRownames(x)) {
            rownames <- NULL
        }
        as_tibble(x = x, ..., rownames = rownames)
    }

formals(as_tibble.IPosRanges)[["rownames"]] <- rownames



#' @rdname coerce-tbl_df
#' @export
## Updated 2019-07-20.
as_tibble.GenomicRanges <- as_tibble.IPosRanges  # nolint



rm(rownames)
