#' Coerce to data frame
#'
#' Coerce to `data.frame`.
#'
#' @name coerce-data.frame
#'
#' @inheritParams base::as.data.frame
#' @inheritParams acidroxygen::params
#'
#' @return `data.frame`.
#'
#' @examples
#' data(sparseMatrix, package = "acidtest")
#'
#' ## Matrix to data.frame ====
#' x <- as(sparseMatrix, "data.frame")
#' head(x)
NULL



`coerce,ANY,data.frame` <-  # nolint
    function(from) {
        as.data.frame(from)
    }



## Updated 2019-07-19.
`coerce,IPosRanges,data.frame` <-  # nolint
    `coerce,ANY,data.frame`



#' @rdname coerce-data.frame
#' @name coerce,IPosRanges,data.frame-method
setAs(
    from = "IPosRanges",
    to = "data.frame",
    def = `coerce,IPosRanges,data.frame`
)



## Updated 2019-07-20.
`coerce,Matrix,data.frame` <-  # nolint
    `coerce,ANY,data.frame`



#' @rdname coerce-data.frame
#' @name coerce,Matrix,data.frame-method
setAs(
    from = "Matrix",
    to = "data.frame",
    def = `coerce,Matrix,data.frame`
)
