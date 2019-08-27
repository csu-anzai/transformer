#' Coerce to data table
#'
#' Coerce to `data.table`.
#'
#' @name coerce-data.table
#'
#' @inheritParams data.table::as.data.table
#' @inheritParams acidroxygen::params
#'
#' @details
#' Our defined methods attempt to improve on the defaults in the data.table
#' package to ensure that row names are not dropped by default, which is a poor
#' default for bioinformatics. This is accomplished by setting
#' `keep.rownames = "rowname"` by default instead of `keep.rownames = NULL`.
#' Note that we're manually defining the `"rowname"` column instead of using
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
#' @return `data.table`.
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



## Updated 2019-07-19.
`coerce,ANY,data.table` <-  # nolint
    function(from) {
        as.data.table(from)
    }


## Updated 2019-07-19.
`coerce,data.frame,data.table` <-  # nolint
    `coerce,ANY,data.table`



#' @rdname coerce-data.table
#' @name coerce,data.frame,data.table-method
setAs(
    from = "data.frame",
    to = "data.table",
    def = `coerce,data.frame,data.table`
)



## Updated 2019-07-19.
`coerce,DataFrame,data.table` <-  # nolint
    `coerce,ANY,data.table`



#' @rdname coerce-data.table
#' @name coerce,DataFrame,data.table-method
setAs(
    from = "DataFrame",
    to = "data.table",
    def = `coerce,DataFrame,data.table`
)



## Updated 2019-07-19.
`coerce,IPosRanges,data.table` <-  # nolint
    `coerce,ANY,data.table`



#' @rdname coerce-data.table
#' @name coerce,IPosRanges,data.table-method
setAs(
    from = "IPosRanges",
    to = "data.table",
    def = `coerce,IPosRanges,data.table`
)



## Updated 2019-07-20.
`coerce,GenomicRanges,data.table` <-  # nolint
    `coerce,ANY,data.table`



#' @rdname coerce-data.table
#' @name coerce,GenomicRanges,data.table-method
setAs(
    from = "GenomicRanges",
    to = "data.table",
    def = `coerce,GenomicRanges,data.table`
)
