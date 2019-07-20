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
