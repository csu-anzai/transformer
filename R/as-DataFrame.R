#' Coerce to `DataFrame`
#'
#' @name as-DataFrame
#'
#' @examples
#' ## sparseMatrix ====
#' load(system.file("extdata", "sparseMatrix.rda", package = "S4Transformer"))
#' x <- as(sparseMatrix, "DataFrame")
#'
#' ## tbl_df ====
#' load(system.file("extdata", "tbl_df.rda", package = "S4Transformer"))
#' x <- as(tbl_df, "DataFrame")
#' head(x)
NULL



# S3 (internal) ================================================================
as.DataFrame <-  # nolint
    function(x) {
        UseMethod("as.DataFrame")
    }



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
#' @rdname as-DataFrame
#' @name coerce,sparseMatrix,DataFrame-method
setAs(
    from = "sparseMatrix",
    to = "DataFrame",
    def = function(from) {
        as.DataFrame(from)
    }
)



#' @rdname as-DataFrame
#' @name coerce,tbl_df,DataFrame-method
setAs(
    from = "tbl_df",
    to = "DataFrame",
    def = function(from) {
        as.DataFrame(from)
    }
)
