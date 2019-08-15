#' @inherit dplyr::arrange_all title description details
#'
#' @note These functions will drop row names, intentionally.
#'
#' @section `data.frame` methods:
#'
#' Since we are defining S4 methods in this package, we are providing
#' passthrough support to dplyr for `data.frame` class objects. Our generic
#' methods pass through to dplyr arrange functions, which are optimized for
#' `tbl_df` class.
#'
#' Refer to `help(topic = "arrange_all", package = "dplyr")` for details.
#'
#' @name arrange
#' @note Updated 2019-08-15.
#'
#' @return Modified object, with rows variably sorted per column.
#'
#' @examples
#' data(mtcars, package = "datasets")
#'
#' ## DataFrame ====
#' x <- as(mtcars, "DataFrame")
#' print(x)
#' x %>% arrange_all(desc)
#' x %>% arrange_if(is.integer, desc)
#' x %>% arrange_at(c("mpg", "cyl"), desc)
NULL



`arrange_all,data.frame` <-  # nolint
    function(.tbl, .funs = list(), ...) {
        requireNamespace("dplyr", quietly = TRUE)
        dplyr::arrange_all(
            .tbl = .tbl,
            .funs = .funs,
            ...
        )
    }



#' @rdname arrange
#' @export
setMethod(
    f = "arrange_all",
    signature = signature("data.frame"),
    definition = `arrange_all,data.frame`
)



`arrange_all,DataFrame` <-  # nolint
    function(.tbl, .funs = list(), ...) {
        tbl <- arrange_all(
            .tbl = as_tibble(.tbl, rownames = NULL),
            .funs = .funs,
            ...
        )
        out <- as(tbl, "DataFrame")
        out
    }



#' @rdname arrange
#' @export
setMethod(
    f = "arrange_all",
    signature = signature("DataFrame"),
    definition = `arrange_all,DataFrame`
)



`arrange_at,data.frame` <-  # nolint
    function(.tbl, .vars, .funs = list(), ...) {
        requireNamespace("dplyr", quietly = TRUE)
        dplyr::arrange_at(
            .tbl = .tbl,
            .vars = .vars,
            .funs = .funs,
            ...
        )
    }



#' @rdname arrange
#' @export
setMethod(
    f = "arrange_at",
    signature = signature("data.frame"),
    definition = `arrange_at,data.frame`
)



`arrange_at,DataFrame` <-  # nolint
    function(.tbl, .vars, .funs = list(), ...) {
        tbl <- arrange_at(
            .tbl = as_tibble(.tbl, rownames = NULL),
            .vars = .vars,
            .funs = .funs,
            ...
        )
        out <- as(tbl, "DataFrame")
        out
    }



#' @rdname arrange
#' @export
setMethod(
    f = "arrange_at",
    signature = signature("DataFrame"),
    definition = `arrange_at,DataFrame`
)



`arrange_if,data.frame` <-  # nolint
    function(.tbl, .predicate, .funs = list(), ...) {
        requireNamespace("dplyr", quietly = TRUE)
        dplyr::arrange_if(
            .tbl = .tbl,
            .predicate = .predicate,
            .funs = .funs,
            ...
        )
    }



#' @rdname arrange
#' @export
setMethod(
    f = "arrange_if",
    signature = signature("data.frame"),
    definition = `arrange_if,data.frame`
)



`arrange_if,DataFrame` <-  # nolint
    function(.tbl, .predicate, .funs = list(), ...) {
        tbl <- arrange_if(
            .tbl = as_tibble(.tbl, rownames = NULL),
            .predicate = .predicate,
            .funs = .funs,
            ...
        )
        out <- as(tbl, "DataFrame")
        out
    }



#' @rdname arrange
#' @export
setMethod(
    f = "arrange_if",
    signature = signature("DataFrame"),
    definition = `arrange_if,DataFrame`
)
