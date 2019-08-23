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
#' selectAll(x, .funs = toupper)
#' selectAt(x, .vars = c("mpg", "cyl"))
#' selectIf(x, .predicate = is.double)
NULL



`selectAll,data.frame` <-  # nolint
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
    f = "selectAll",
    signature = signature("data.frame"),
    definition = `selectAll,data.frame`
)



`selectAll,DataFrame` <-  # nolint
    function(.tbl, .funs = list(), ...) {
        tbl <- selectAll(
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
    f = "selectAll",
    signature = signature("DataFrame"),
    definition = `selectAll,DataFrame`
)



`selectAt,data.frame` <-  # nolint
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
    f = "selectAt",
    signature = signature("data.frame"),
    definition = `selectAt,data.frame`
)



`selectAt,DataFrame` <-  # nolint
    function(.tbl, .vars, .funs = list(), ...) {
        tbl <- selectAt(
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
    f = "selectAt",
    signature = signature("DataFrame"),
    definition = `selectAt,DataFrame`
)



`selectIf,data.frame` <-  # nolint
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
    f = "selectIf",
    signature = signature("data.frame"),
    definition = `selectIf,data.frame`
)



`selectIf,DataFrame` <-  # nolint
    function(.tbl, .predicate, .funs = list(), ...) {
        tbl <- selectIf(
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
    f = "selectIf",
    signature = signature("DataFrame"),
    definition = `selectIf,DataFrame`
)
