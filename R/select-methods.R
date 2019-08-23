## FIXME Rework these methods



#' @inherit dplyr::select_all title
#'
#' @section `data.frame` methods:
#'
#' Since we are defining S4 methods in this package, we are providing
#' passthrough support to dplyr for `data.frame` class objects. Our generic
#' methods pass through to dplyr select functions, which are optimized for
#' `tbl_df` class.
#'
#' Refer to `help(topic = "select_all", package = "dplyr")` for details.
#'
#' @name select
#' @note Updated 2019-08-15.
#'
#' @inheritParams mutate
#'
#' @return Modified object.
#'
#' @examples
#' data(mtcars, package = "datasets")
#'
#' ## DataFrame ====
#' x <- as(mtcars, "DataFrame")
#' selectAll(x, funs = toupper)
#' selectAt(x, .vars = c("mpg", "cyl"))
#' selectIf(x, predicate = is.double)
NULL



`selectAll,DataFrame` <-  # nolint
    function(object, funs = list(), ...) {
        ## FIXME
        stop("REWORK")
    }



#' @rdname select
#' @export
setMethod(
    f = "selectAll",
    signature = signature("DataFrame"),
    definition = `selectAll,DataFrame`
)



`selectAt,DataFrame` <-  # nolint
    function(object, .vars, funs = list(), ...) {
        ## FIXME
        stop("REWORK")
    }



#' @rdname select
#' @export
setMethod(
    f = "selectAt",
    signature = signature("DataFrame"),
    definition = `selectAt,DataFrame`
)



`selectIf,DataFrame` <-  # nolint
    function(object, predicate, funs = list(), ...) {
        ## FIXME
        stop("REWORK")
    }



#' @rdname select
#' @export
setMethod(
    f = "selectIf",
    signature = signature("DataFrame"),
    definition = `selectIf,DataFrame`
)
