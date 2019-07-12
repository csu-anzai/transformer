#' Coerce to `DataFrame`
#'
#' @name coerce-DataFrame
#' @inheritParams params
#'
#' @examples
#' ## sparseMatrix to DataFrame ====
#' data(sparse, package = "acidtest")
#' stopifnot(is(sparse, "sparseMatrix"))
#' x <- as(sparse, "DataFrame")
#'
#' ## tbl_df to DataFrame ====
#' data(tbl, package = "acidtest")
#' stopifnot(is(tbl, "tbl_df"))
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



# Updated 2019-07-12.
#' @method as.DataFrame default
#' @export
as.DataFrame.default <- function(x) {
    to <- as.data.frame(x, stringsAsFactors = FALSE)
    to <- as(to, "DataFrame")

    # Automatically move row names column, if defined.
    # Currently provides support for data.table and tibble.
    # data.table: "rn"
    # tibble: "rowname"
    rncol <- na.omit(match(x = c("rowname", "rn"), table = colnames(to)))
    if (length(rncol) == 1L) {
        rownames <- as.character(to[[rncol]])
        assert(hasNoDuplicates(rownames))
        rownames(to) <- rownames
        to[[rncol]] <- NULL
    } else if (length(rncol) > 1L) {
        stop(paste(
            "Multiple row names columns detected:",
            toString(colnames(to)[rncol])
        ))
    }

    to
}



# S4 ===========================================================================
# Updated 2019-07-12.
#' @rdname coerce-DataFrame
#' @name coerce,data.table,DataFrame-method
setAs(
    from = "data.table",
    to = "DataFrame",
    def = function(from) {
        as.DataFrame(from)
    }
)



# Updated 2019-07-12.
#' @rdname coerce-DataFrame
#' @name coerce,sparseMatrix,DataFrame-method
setAs(
    from = "sparseMatrix",
    to = "DataFrame",
    def = function(from) {
        as.DataFrame(from)
    }
)



# Updated 2019-07-12.
#' @rdname coerce-DataFrame
#' @name coerce,tbl_df,DataFrame-method
setAs(
    from = "tbl_df",
    to = "DataFrame",
    def = function(from) {
        as.DataFrame(from)
    }
)
