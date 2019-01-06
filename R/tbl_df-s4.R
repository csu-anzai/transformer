#' @rdname as
#' @name coerce,data.frame,tbl_df-method
setAs(
    from = "data.frame",
    to = "tbl_df",
    def = function(from) {
        as_tibble(from)
    }
)

#' @rdname as
#' @name coerce,DataFrame,tbl_df-method
setAs(
    from = "DataFrame",
    to = "tbl_df",
    def = function(from) {
        as_tibble(from)
    }
)

#' @rdname as
#' @name coerce,GRanges,tbl_df-method
setAs(
    from = "GRanges",
    to = "tbl_df",
    def = function(from) {
        as_tibble(from)
    }
)
