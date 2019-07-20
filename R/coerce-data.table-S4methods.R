# Updated 2019-07-19.
`coerce,ANY,data.table` <-  # nolint
    function(from) {
        as.data.table(from)
    }


# Updated 2019-07-19.
`coerce,data.frame,data.table` <-  # nolint
    `coerce,ANY,data.table`



#' @rdname coerce-data.table
#' @name coerce,data.frame,data.table-method
setAs(
    from = "data.frame",
    to = "data.table",
    def = `coerce,data.frame,data.table`
)



# Updated 2019-07-19.
`coerce,DataFrame,data.table` <-  # nolint
    `coerce,ANY,data.table`



#' @rdname coerce-data.table
#' @name coerce,DataFrame,data.table-method
setAs(
    from = "DataFrame",
    to = "data.table",
    def = `coerce,DataFrame,data.table`
)



# Updated 2019-07-19.
`coerce,GRanges,data.table` <-  # nolint
    `coerce,ANY,data.table`



#' @rdname coerce-data.table
#' @name coerce,GRanges,data.table-method
setAs(
    from = "GRanges",
    to = "data.table",
    def = `coerce,GRanges,data.table`
)



# Updated 2019-07-19.
`coerce,IPosRanges,data.table` <-  # nolint
    `coerce,ANY,data.table`



#' @rdname coerce-data.table
#' @name coerce,IPosRanges,data.table-method
setAs(
    from = "IPosRanges",
    to = "data.table",
    def = `coerce,IPosRanges,data.table`
)
