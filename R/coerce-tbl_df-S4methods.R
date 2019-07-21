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
