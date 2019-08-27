#' Coerce to tibble
#'
#' Coerce to `tbl_df`.
#'
#' @name coerce-tbl_df
#'
#' @inheritParams tibble::as_tibble
#' @inheritParams acidroxygen::params
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
#' @return `tbl_df`.
#'
#' @examples
#' data(DataFrame, GRanges, IRanges, package = "acidtest")
#'
#' ## DataFrame to tbl_df ====
#' x <- as(DataFrame, "tbl_df")
#' x <- as_tibble(DataFrame)
#' print(x)
#'
#' ## GenomicRanges to tbl_df ====
#' x <- as(GRanges, "tbl_df")
#' x <- as_tibble(GRanges)
#' print(x)
#'
#' ## IRanges to tbl_df ====
#' x <- as(IRanges, "tbl_df")
#' x <- as_tibble(IRanges)
#' print(x)
NULL



## Updated 2019-07-19.
`coerce,ANY,tbl_df` <-  # nolint
    function(from) {
        as_tibble(from)
    }



## Updated 2019-07-19.
`coerce,data.frame,tbl_df` <-  # nolint
    `coerce,ANY,tbl_df`



#' @rdname coerce-tbl_df
#' @name coerce,data.frame,tbl_df-method
setAs(
    from = "data.frame",
    to = "tbl_df",
    def = `coerce,data.frame,tbl_df`
)



## Updated 2019-07-19.
`coerce,DataFrame,tbl_df` <-  # nolint
    `coerce,ANY,tbl_df`



#' @rdname coerce-tbl_df
#' @name coerce,DataFrame,tbl_df-method
setAs(
    from = "DataFrame",
    to = "tbl_df",
    def = `coerce,DataFrame,tbl_df`
)



## Updated 2019-07-20.
`coerce,GenomicRanges,tbl_df` <-  # nolint
    `coerce,ANY,tbl_df`



#' @rdname coerce-tbl_df
#' @name coerce,GenomicRanges,tbl_df-method
setAs(
    from = "GenomicRanges",
    to = "tbl_df",
    def = `coerce,GenomicRanges,tbl_df`
)



## Updated 2019-07-20.
`coerce,IPosRanges,tbl_df` <-  # nolint
    `coerce,ANY,tbl_df`



#' @rdname coerce-tbl_df
#' @name coerce,IPosRanges,tbl_df-method
setAs(
    from = "IPosRanges",
    to = "tbl_df",
    def = `coerce,IPosRanges,tbl_df`
)
