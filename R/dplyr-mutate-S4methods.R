#' @inherit dplyr::mutate_all title
#'
#' @section `data.frame` methods:
#'
#' Since we are defining S4 methods in this package, we are providing
#' passthrough support to dplyr for `data.frame` class objects. Our generic
#' methods pass through to dplyr mutate functions, which are optimized for
#' `tbl_df` class.
#'
#' Refer to `help(topic = "mutate_all", package = "dplyr")` for details.
#'
#' @name mutate
#' @note Updated 2019-08-15.
#'
#' @param .tbl Object.
#' @param .funs,.predicate `function`.
#'   Refer to dplyr documentation for details.
#' @param .vars `character`.
#'   Column names.
#' @param ... Additional argument.
#'
#' @return Modified object.
#'
#' @examples
#' data(mtcars, package = "datasets")
#'
#' ## DataFrame ====
#' x <- as(mtcars, "DataFrame")
#' mutate_all(x, .funs = log, base = 2L)
#' mutate_at(x, .vars = c("mpg", "cyl"), log, base = 2L)
#' mutate_if(x, .predicate = is.double, .funs = as.integer)
#' transmute_at(x, .vars = c("mpg", "cyl"), log, base = 2L)
#' transmute_if(x, .predicate = is.double, .funs = as.integer)
NULL



`mutate_all,data.frame` <-  # nolint
    function(.tbl, .funs, ...) {
        requireNamespace("dplyr", quietly = TRUE)
        dplyr::mutate_all(
            .tbl = .tbl,
            .funs = .funs,
            ...
        )
    }



#' @rdname mutate
#' @export
setMethod(
    f = "mutate_all",
    signature = signature("data.frame"),
    definition = `mutate_all,data.frame`
)



`mutate_all,DataFrame` <-  # nolint
    function(.tbl, .funs, ...) {
        tbl <- mutate_all(
            .tbl = as_tibble(.tbl, rownames = NULL),
            .funs = .funs,
            ...
        )
        out <- as(tbl, "DataFrame")
        rownames(out) <- rownames(.tbl)
        out
    }



#' @rdname mutate
#' @export
setMethod(
    f = "mutate_all",
    signature = signature("DataFrame"),
    definition = `mutate_all,DataFrame`
)



`mutate_at,data.frame` <-  # nolint
    function(.tbl, .vars, .funs, ...) {
        requireNamespace("dplyr", quietly = TRUE)
        dplyr::mutate_at(
            .tbl = .tbl,
            .vars = .vars,
            .funs = .funs,
            ...
        )
    }



#' @rdname mutate
#' @export
setMethod(
    f = "mutate_at",
    signature = signature("data.frame"),
    definition = `mutate_at,data.frame`
)



`mutate_at,DataFrame` <-  # nolint
    function(.tbl, .vars, .funs, ...) {
        tbl <- mutate_at(
            .tbl = as_tibble(.tbl, rownames = NULL),
            .vars = .vars,
            .funs = .funs,
            ...
        )
        out <- as(tbl, "DataFrame")
        rownames(out) <- rownames(.tbl)
        out
    }



#' @rdname mutate
#' @export
setMethod(
    f = "mutate_at",
    signature = signature("DataFrame"),
    definition = `mutate_at,DataFrame`
)



`mutate_if,data.frame` <-  # nolint
    function(.tbl, .predicate, .funs, ...) {
        requireNamespace("dplyr", quietly = TRUE)
        dplyr::mutate_if(
            .tbl = .tbl,
            .predicate = .predicate,
            .funs = .funs,
            ...
        )
    }



#' @rdname mutate
#' @export
setMethod(
    f = "mutate_if",
    signature = signature("data.frame"),
    definition = `mutate_if,data.frame`
)



`mutate_if,DataFrame` <-  # nolint
    function(.tbl, .predicate, .funs, ...) {
        tbl <- mutate_if(
            .tbl = as_tibble(.tbl, rownames = NULL),
            .predicate = .predicate,
            .funs = .funs,
            ...
        )
        out <- as(tbl, "DataFrame")
        rownames(out) <- rownames(.tbl)
        out
    }



#' @rdname mutate
#' @export
setMethod(
    f = "mutate_if",
    signature = signature("DataFrame"),
    definition = `mutate_if,DataFrame`
)



`transmute_at,data.frame` <-  # nolint
    function(.tbl, .vars, .funs, ...) {
        requireNamespace("dplyr", quietly = TRUE)
        dplyr::transmute_at(
            .tbl = .tbl,
            .vars = .vars,
            .funs = .funs,
            ...
        )
    }



#' @rdname mutate
#' @export
setMethod(
    f = "transmute_at",
    signature = signature("data.frame"),
    definition = `transmute_at,data.frame`
)



`transmute_at,DataFrame` <-  # nolint
    function(.tbl, .vars, .funs, ...) {
        tbl <- transmute_at(
            .tbl = as_tibble(.tbl, rownames = NULL),
            .vars = .vars,
            .funs = .funs,
            ...
        )
        out <- as(tbl, "DataFrame")
        rownames(out) <- rownames(.tbl)
        out
    }



#' @rdname mutate
#' @export
setMethod(
    f = "transmute_at",
    signature = signature("DataFrame"),
    definition = `transmute_at,DataFrame`
)



`transmute_if,data.frame` <-  # nolint
    function(.tbl, .predicate, .funs, ...) {
        requireNamespace("dplyr", quietly = TRUE)
        dplyr::transmute_if(
            .tbl = .tbl,
            .predicate = .predicate,
            .funs = .funs,
            ...
        )
    }



#' @rdname mutate
#' @export
setMethod(
    f = "transmute_if",
    signature = signature("data.frame"),
    definition = `transmute_if,data.frame`
)



`transmute_if,DataFrame` <-  # nolint
    function(.tbl, .predicate, .funs, ...) {
        tbl <- transmute_if(
            .tbl = as_tibble(.tbl, rownames = NULL),
            .predicate = .predicate,
            .funs = .funs,
            ...
        )
        out <- as(tbl, "DataFrame")
        rownames(out) <- rownames(.tbl)
        out
    }



#' @rdname mutate
#' @export
setMethod(
    f = "transmute_if",
    signature = signature("DataFrame"),
    definition = `transmute_if,DataFrame`
)
