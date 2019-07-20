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
#' ## Matrix to DataFrame ====
#' x <- as(sparse, "DataFrame")
#'
#' ## tbl_df to DataFrame ====
#' x <- as(tbl, "DataFrame")
#' head(x)
NULL



# Updated 2019-07-19.
`coerce,ANY,DataFrame` <- function(from) {
    to <- as.data.frame(from, stringsAsFactors = FALSE)
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



# Updated 2019-07-12.
`coerce,data.table,DataFrame` <-  # nolint
    `coerce,ANY,DataFrame`



#' @rdname coerce-DataFrame
#' @name coerce,data.table,DataFrame-method
setAs(
    from = "data.table",
    to = "DataFrame",
    def = `coerce,data.table,DataFrame`
)



# Updated 2019-07-12.
`coerce,Matrix,DataFrame` <-  # nolint
    `coerce,ANY,DataFrame`



#' @rdname coerce-DataFrame
#' @name coerce,Matrix,DataFrame-method
setAs(
    from = "Matrix",
    to = "DataFrame",
    def = `coerce,Matrix,DataFrame`
)



# Updated 2019-07-12.
`coerce,tbl_df,DataFrame` <-  # nolint
    `coerce,ANY,DataFrame`



#' @rdname coerce-DataFrame
#' @name coerce,tbl_df,DataFrame-method
setAs(
    from = "tbl_df",
    to = "DataFrame",
    def = `coerce,tbl_df,DataFrame`
)
