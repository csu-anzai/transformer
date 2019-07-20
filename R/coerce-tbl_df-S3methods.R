#' Coerce to tibble
#'
#' Coerce to `tbl_df`.
#'
#' @name coerce-tbl_df
#' @inheritParams tibble::as_tibble
#' @inheritParams params
#'
#' @details
#' Our defined methods attempt to improve on the defaults in the tibble package
#' to ensure that row names are not dropped by default, which is a poor default
#' for bioinformatics. This is accomplished by setting `rownames = "rowname"` by
#' default instead of `rownames = NULL`.
#'
#' @section S3 `as_tibble()`:
#'
#' transformer extends [`as_tibble()`][tibble::as_tibble] method support for
#' these S4 classes:
#'
#' - `DataFrame`.
#' - `GRanges`.
#'
#' @section S4 `as()`:
#'
#' Since `tbl_df` is a virtual class that extends `tbl` and `data.frame`, we
#' need to define an S4 coercion method that allows us to use
#' [`as()`][methods::as] to coerce an object to a tibble.
#'
#' See `getClass("tbl_df")` for details on how tibble is a virtual class.
#'
#' @seealso [tibble::as_tibble()].
#'
#' @examples
#' data(df, gr, ir, package = "acidtest")
#'
#' ## DataFrame to tbl_df ====
#' x <- as(df, "tbl_df")
#' x <- as_tibble(df)
#' print(x)
#'
#' ## GenomicRanges to tbl_df ====
#' x <- as(gr, "tbl_df")
#' x <- as_tibble(gr)
#' print(x)
#'
#' ## IRanges to tbl_df ====
#' x <- as(ir, "tbl_df")
#' x <- as_tibble(ir)
#' print(x)
NULL



rownames <- quote(pkgconfig::get_config("tibble::rownames", "rowname"))



#' @rdname coerce-tbl_df
#' @name as_tibble
#' @importFrom tibble as_tibble
#' @export
NULL



#' @rdname coerce-tbl_df
#' @export
# Updated 2019-07-19.
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
# Updated 2019-07-20.
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
# Updated 2019-07-20.
as_tibble.GenomicRanges <- as_tibble.IPosRanges  # nolint



rm(rownames)
