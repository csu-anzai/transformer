#' Coerce to data frame
#'
#' Coerce to `DataFrame`.
#'
#' @name coerce-DataFrame
#' @inheritParams params
#'
#' @examples
#' data(dt, sparse, tbl, package = "acidtest")
#'
#' ## data.table to DataFrame ====
#' x <- as(dt, "DataFrame")
#' head(x)
#'
#' ## sparseMatrix to DataFrame ====
#' x <- as(sparse, "DataFrame")
#'
#' ## tbl_df to DataFrame ====
#' x <- as(tbl, "DataFrame")
#' head(x)
NULL



# S3 ===========================================================================
#' @rdname coerce-DataFrame
#' @export
as.DataFrame <-  # nolint
    function(x) {
        UseMethod("as.DataFrame")
    }



#' @rdname coerce-DataFrame
#' @export
# Updated 2019-07-19.
as.DataFrame.default <- function(x) {
    to <- as.data.frame(x, stringsAsFactors = FALSE)
    to <- as(to, "DataFrame")

    # Move row names automatically, if defined.
    if (!hasRownames(to)) {
        rncol <- matchRowNameColumn(to)
        if (is.character(rncol) && length(rncol) == 1L) {
            rownames(to) <- as.character(to[[rncol]])
            to[[rncol]] <- NULL
        }
    }

    to
}



# S4 ===========================================================================
# Updated 2019-07-12.
`coerce,data.table,DataFrame` <-  # nolint
    function(from) {
        as.DataFrame(from)
    }



#' @rdname coerce-DataFrame
#' @name coerce,data.table,DataFrame-method
setAs(
    from = "data.table",
    to = "DataFrame",
    def = `coerce,data.table,DataFrame`
)



# Updated 2019-07-12.
`coerce,sparseMatrix,DataFrame` <-  # nolint
    `coerce,data.table,DataFrame`



#' @rdname coerce-DataFrame
#' @name coerce,sparseMatrix,DataFrame-method
setAs(
    from = "sparseMatrix",
    to = "DataFrame",
    def = `coerce,sparseMatrix,DataFrame`
)



# Updated 2019-07-12.
`coerce,tbl_df,DataFrame` <-  # nolint
    `coerce,data.table,DataFrame`



#' @rdname coerce-DataFrame
#' @name coerce,tbl_df,DataFrame-method
setAs(
    from = "tbl_df",
    to = "DataFrame",
    def = `coerce,tbl_df,DataFrame`
)
