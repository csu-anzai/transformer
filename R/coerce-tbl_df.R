#' Coerce to tibble
#'
#' Coerce to `tbl_df`.
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
#' data(df, gr, ir, package = "acidtest")
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
#'
#' ## IRanges to tbl_df ====
#' x <- as(ir, "tbl_df")
#' x <- as_tibble(ir)
#' print(x)
NULL



# S3 ===========================================================================
#' @rdname coerce-tbl_df
#' @name as_tibble
#' @importFrom tibble as_tibble
#' @export
NULL

rownames <- quote(pkgconfig::get_config("tibble::rownames", "rowname"))



#' @rdname coerce-tbl_df
#' @export
# Updated 2019-07-19.
as_tibble.DataFrame <-  # nolint
    function(x, ..., rownames) {
        x <- `.coerce.DataFrame,data.frame`(x)
        if (!hasRownames(x)) {
            rownames <- NULL
        }
        as_tibble(x = x, ..., rownames = rownames)
    }

formals(as_tibble.DataFrame)[["rownames"]] <- rownames



# The default handling from data.frame isn't clean, so add this.
# Default method will warn: `Arguments in '...' ignored`.

#' @rdname coerce-tbl_df
#' @export
# Updated 2019-07-19.
as_tibble.GRanges <-  # nolint
    function(x, ..., rownames) {
        x <- as(x, "data.frame")
        if (!hasRownames(x)) {
            rownames <- NULL
        }
        as_tibble(x = x, ..., rownames = rownames)
    }

formals(as_tibble.GRanges)[["rownames"]] <- rownames



#' @rdname coerce-tbl_df
#' @export
# Updated 2019-07-12.
as_tibble.IRanges <- as_tibble.GRanges  # nolint



# S4 ===========================================================================
# Updated 2019-07-19.
`coerce,ANY,tbl_df` <-  # nolint
    function(from) {
        as_tibble(from)
    }



# Updated 2019-07-19.
`coerce,data.frame,tbl_df` <-  # nolint
    `coerce,ANY,tbl_df`



#' @rdname coerce-tbl_df
#' @name coerce,data.frame,tbl_df-method
setAs(
    from = "data.frame",
    to = "tbl_df",
    def = `coerce,data.frame,tbl_df`
)



# Updated 2019-07-19.
`coerce,DataFrame,tbl_df` <-  # nolint
    `coerce,ANY,tbl_df`



#' @rdname coerce-tbl_df
#' @name coerce,DataFrame,tbl_df-method
setAs(
    from = "DataFrame",
    to = "tbl_df",
    def = `coerce,DataFrame,tbl_df`
)



# Updated 2019-07-19.
`coerce,GRanges,tbl_df` <-  # nolint
    `coerce,ANY,tbl_df`



#' @rdname coerce-tbl_df
#' @name coerce,GRanges,tbl_df-method
setAs(
    from = "GRanges",
    to = "tbl_df",
    def = `coerce,GRanges,tbl_df`
)



# Updated 2019-07-19.
`coerce,IRanges,tbl_df` <-  # nolint
    `coerce,ANY,tbl_df`



#' @rdname coerce-tbl_df
#' @name coerce,IRanges,tbl_df-method
setAs(
    from = "IRanges",
    to = "tbl_df",
    def = `coerce,IRanges,tbl_df`
)
