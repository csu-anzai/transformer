#' Coerce to `tbl_df` (tibble)
#'
#' @name coerce-tbl_df
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
#' data(rse, package = "acidtest")
#' stopifnot(is(rse, "RangedSummarizedExperiment"))
#'
#' df <- SummarizedExperiment::colData(rse)
#' gr <- SummarizedExperiment::rowRanges(rse)
#'
#' ## DataFrame to tbl_df ====
#' x <- as(df, "tbl_df")
#' x <- as_tibble(df)
#' print(x)
#'
#' ## GRanges to tbl_df ====
#' x <- as(gr, "tbl_df")
#' x <- as_tibble(gr)
#' print(x)
NULL



# S3 ===========================================================================
#' @rdname coerce-tbl_df
#' @name as_tibble
#' @importFrom tibble as_tibble
#' @export
NULL

rownames <- quote(pkgconfig::get_config("tibble::rownames", "rowname"))



# Updated 2019-07-11.
#' @method as_tibble DataFrame
#' @export
as_tibble.DataFrame <-  # nolint
    function(x, ..., rownames) {
        x <- .coerceDataFrame(x)
        if (!hasRownames(x)) {
            rownames <- NULL
        }
        as_tibble(x = x, ..., rownames = rownames)
    }

formals(as_tibble.DataFrame)[["rownames"]] <- rownames



# The default handling from data.frame isn't clean, so add this.
# Default method will warn: `Arguments in '...' ignored`.
# Updated 2019-07-11.
#' @method as_tibble GRanges
#' @export
as_tibble.GRanges <-  # nolint
    function(x, ..., rownames) {
        x <- as(x, "data.frame")
        if (!hasRownames(x)) {
            rownames <- NULL
        }
        as_tibble(x = x, ..., rownames = rownames)
    }

formals(as_tibble.GRanges)[["rownames"]] <- rownames



# Updated 2019-07-11.
#' @method as_tibble IRanges
#' @export
as_tibble.IRanges <- as_tibble.GRanges



# S4 ===========================================================================
# Updated 2019-07-11.
#' @rdname coerce-tbl_df
#' @name coerce,data.frame,tbl_df-method
setAs(
    from = "data.frame",
    to = "tbl_df",
    def = function(from) {
        as_tibble(from)
    }
)



# Updated 2019-07-11.
#' @rdname coerce-tbl_df
#' @name coerce,DataFrame,tbl_df-method
setAs(
    from = "DataFrame",
    to = "tbl_df",
    def = function(from) {
        as_tibble(from)
    }
)



# Updated 2019-07-11.
#' @rdname coerce-tbl_df
#' @name coerce,GRanges,tbl_df-method
setAs(
    from = "GRanges",
    to = "tbl_df",
    def = function(from) {
        as_tibble(from)
    }
)



# Updated 2019-07-11.
#' @rdname coerce-tbl_df
#' @name coerce,IRanges,tbl_df-method
setAs(
    from = "IRanges",
    to = "tbl_df",
    def = function(from) {
        as_tibble(from)
    }
)
