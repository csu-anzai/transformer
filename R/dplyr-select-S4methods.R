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
#' select_all(x, .funs = toupper)
#' select_at(x, .vars = c("mpg", "cyl"))
#' select_if(x, .predicate = is.double)
NULL



`select_all,data.frame` <-  # nolint
    function(.tbl, .funs = list(), ...) {
        assert(requireNamespace("dplyr", quietly = TRUE))
        dplyr::select_all(
            .tbl = .tbl,
            .funs = .funs,
            ...
        )
    }



#' @rdname select
#' @export
setMethod(
    f = "select_all",
    signature = signature("data.frame"),
    definition = `select_all,data.frame`
)



`select_all,DataFrame` <-  # nolint
    function(.tbl, .funs = list(), ...) {
        tbl <- select_all(
            .tbl = as_tibble(.tbl, rownames = NULL),
            .funs = .funs,
            ...
        )
        out <- as(tbl, "DataFrame")
        rownames(out) <- rownames(.tbl)
        out
    }



#' @rdname select
#' @export
setMethod(
    f = "select_all",
    signature = signature("DataFrame"),
    definition = `select_all,DataFrame`
)



`select_at,data.frame` <-  # nolint
    function(.tbl, .vars, .funs = list(), ...) {
        assert(requireNamespace("dplyr", quietly = TRUE))
        dplyr::select_at(
            .tbl = .tbl,
            .vars = .vars,
            .funs = .funs,
            ...
        )
    }



#' @rdname select
#' @export
setMethod(
    f = "select_at",
    signature = signature("data.frame"),
    definition = `select_at,data.frame`
)



`select_at,DataFrame` <-  # nolint
    function(.tbl, .vars, .funs = list(), ...) {
        tbl <- select_at(
            .tbl = as_tibble(.tbl, rownames = NULL),
            .vars = .vars,
            .funs = .funs,
            ...
        )
        out <- as(tbl, "DataFrame")
        rownames(out) <- rownames(.tbl)
        out
    }



#' @rdname select
#' @export
setMethod(
    f = "select_at",
    signature = signature("DataFrame"),
    definition = `select_at,DataFrame`
)



`select_if,data.frame` <-  # nolint
    function(.tbl, .predicate, .funs = list(), ...) {
        assert(requireNamespace("dplyr", quietly = TRUE))
        dplyr::select_if(
            .tbl = .tbl,
            .predicate = .predicate,
            .funs = .funs,
            ...
        )
    }



#' @rdname select
#' @export
setMethod(
    f = "select_if",
    signature = signature("data.frame"),
    definition = `select_if,data.frame`
)



`select_if,DataFrame` <-  # nolint
    function(.tbl, .predicate, .funs = list(), ...) {
        tbl <- select_if(
            .tbl = as_tibble(.tbl, rownames = NULL),
            .predicate = .predicate,
            .funs = .funs,
            ...
        )
        out <- as(tbl, "DataFrame")
        rownames(out) <- rownames(.tbl)
        out
    }



#' @rdname select
#' @export
setMethod(
    f = "select_if",
    signature = signature("DataFrame"),
    definition = `select_if,DataFrame`
)
