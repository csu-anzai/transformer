#' Coerce to `DataFrame`
#'
#' @name coerce-DataFrame
#' @inheritParams params
#'
#' @examples
#' ## sparseMatrix to DataFrame ====
#' load(system.file("extdata", "sparseMatrix.rda", package = "transformer"))
#' x <- as(sparseMatrix, "DataFrame")
#'
#' ## tbl_df to DataFrame ====
#' load(system.file("extdata", "tbl_df.rda", package = "transformer"))
#' x <- as(tbl_df, "DataFrame")
#' head(x)
NULL



# S3 ===========================================================================
#' @rdname coerce-DataFrame
#' @export
as.DataFrame <-  # nolint
    function(x) {
        UseMethod("as.DataFrame")
    }



#' @method as.DataFrame default
#' @export
as.DataFrame.default <- function(x) {
    to <- as.data.frame(x, stringsAsFactors = FALSE)
    to <- as(to, "DataFrame")
    rownames <- as.character(to[["rowname"]])
    if (
        length(rownames) > 0L &&
        !any(duplicated(rownames))
    ) {
        rownames(to) <- rownames
        to[["rowname"]] <- NULL
    }
    to
}



# S4 ===========================================================================
#' @rdname coerce-DataFrame
#' @name coerce,sparseMatrix,DataFrame-method
setAs(
    from = "sparseMatrix",
    to = "DataFrame",
    def = function(from) {
        as.DataFrame(from)
    }
)



#' @rdname coerce-DataFrame
#' @name coerce,tbl_df,DataFrame-method
setAs(
    from = "tbl_df",
    to = "DataFrame",
    def = function(from) {
        as.DataFrame(from)
    }
)
