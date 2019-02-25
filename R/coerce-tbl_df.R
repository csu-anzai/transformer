#' Coerce to `tbl_df` (tibble)
#'
#' @name coerce-tbl_df
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
#' @examples
#' load(system.file("extdata", "rse.rda", package = "transformer"))
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



#' @method as_tibble DataFrame
#' @export
as_tibble.DataFrame <-  # nolint
    function(x, ..., rownames = "rowname") {
        x <- .coerceDataFrame(x)
        if (!hasRownames(x)) {
            rownames <- NULL
        }
        as_tibble(x = x, ..., rownames = rownames)
    }



# The default handling from data.frame isn't clean, so add this.
# Default method will warn: `Arguments in '...' ignored`.
#' @method as_tibble GRanges
#' @export
as_tibble.GRanges <-  # nolint
    function(x, ..., rownames = "rowname") {
        names <- names(x)
        x <- as(x, "data.frame")
        rownames(x) <- names
        if (!hasRownames(x)) {
            rownames <- NULL
        }
        as_tibble(x = x, ..., rownames = rownames)
    }



# S4 ===========================================================================
#' @rdname coerce-tbl_df
#' @name coerce,data.frame,tbl_df-method
setAs(
    from = "data.frame",
    to = "tbl_df",
    def = function(from) {
        as_tibble(from)
    }
)



#' @rdname coerce-tbl_df
#' @name coerce,DataFrame,tbl_df-method
setAs(
    from = "DataFrame",
    to = "tbl_df",
    def = function(from) {
        as_tibble(from)
    }
)



#' @rdname coerce-tbl_df
#' @name coerce,GRanges,tbl_df-method
setAs(
    from = "GRanges",
    to = "tbl_df",
    def = function(from) {
        as_tibble(from)
    }
)
