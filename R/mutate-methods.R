## FIXME Rework and rename the formals.



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
#' mutateAll(x, .funs = log, base = 2L)
#' mutateAt(x, .vars = c("mpg", "cyl"), log, base = 2L)
#' mutateIf(x, .predicate = is.double, .funs = as.integer)
#' transmuteAt(x, .vars = c("mpg", "cyl"), log, base = 2L)
#' transmuteIf(x, .predicate = is.double, .funs = as.integer)
NULL



`mutateAll,data.frame` <-  # nolint
    function(.tbl, .funs, ...) {
        assert(requireNamespace("dplyr", quietly = TRUE))
        dplyr::mutate_all(
            .tbl = .tbl,
            .funs = .funs,
            ...
        )
    }



#' @rdname mutate
#' @export
setMethod(
    f = "mutateAll",
    signature = signature("data.frame"),
    definition = `mutateAll,data.frame`
)



`mutateAll,DataFrame` <-  # nolint
    function(.tbl, .funs, ...) {
        tbl <- mutateAll(
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
    f = "mutateAll",
    signature = signature("DataFrame"),
    definition = `mutateAll,DataFrame`
)



`mutateAt,data.frame` <-  # nolint
    function(.tbl, .vars, .funs, ...) {
        assert(requireNamespace("dplyr", quietly = TRUE))
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
    f = "mutateAt",
    signature = signature("data.frame"),
    definition = `mutateAt,data.frame`
)



`mutateAt,DataFrame` <-  # nolint
    function(.tbl, .vars, .funs, ...) {
        tbl <- mutateAt(
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
    f = "mutateAt",
    signature = signature("DataFrame"),
    definition = `mutateAt,DataFrame`
)



`mutateIf,data.frame` <-  # nolint
    function(.tbl, .predicate, .funs, ...) {
        assert(requireNamespace("dplyr", quietly = TRUE))
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
    f = "mutateIf",
    signature = signature("data.frame"),
    definition = `mutateIf,data.frame`
)



`mutateIf,DataFrame` <-  # nolint
    function(.tbl, .predicate, .funs, ...) {
        tbl <- mutateIf(
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
    f = "mutateIf",
    signature = signature("DataFrame"),
    definition = `mutateIf,DataFrame`
)



`transmuteAt,data.frame` <-  # nolint
    function(.tbl, .vars, .funs, ...) {
        assert(requireNamespace("dplyr", quietly = TRUE))
        dplyr::transmuteAt(
            .tbl = .tbl,
            .vars = .vars,
            .funs = .funs,
            ...
        )
    }



#' @rdname mutate
#' @export
setMethod(
    f = "transmuteAt",
    signature = signature("data.frame"),
    definition = `transmuteAt,data.frame`
)



`transmuteAt,DataFrame` <-  # nolint
    function(.tbl, .vars, .funs, ...) {
        tbl <- transmuteAt(
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
    f = "transmuteAt",
    signature = signature("DataFrame"),
    definition = `transmuteAt,DataFrame`
)



`transmuteIf,data.frame` <-  # nolint
    function(.tbl, .predicate, .funs, ...) {
        assert(requireNamespace("dplyr", quietly = TRUE))
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
    f = "transmuteIf",
    signature = signature("data.frame"),
    definition = `transmuteIf,data.frame`
)



`transmuteIf,DataFrame` <-  # nolint
    function(.tbl, .predicate, .funs, ...) {
        tbl <- transmuteIf(
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
    f = "transmuteIf",
    signature = signature("DataFrame"),
    definition = `transmuteIf,DataFrame`
)
