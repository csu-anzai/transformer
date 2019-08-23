## FIXME Rework and rename the formals.
## FIXME How to support `funs()` here?



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
#' @inheritParams acidroxygen::params
#' @param funs,predicate `function`.
#'   Refer to dplyr documentation for details.
#' @param vars `character`.
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
#' mutateAll(x, funs = log, base = 2L)
#' mutateAt(x, vars = c("mpg", "cyl"), log, base = 2L)
#' mutateIf(x, predicate = is.double, funs = as.integer)
#' transmuteAt(x, vars = c("mpg", "cyl"), log, base = 2L)
#' transmuteIf(x, predicate = is.double, funs = as.integer)
NULL



`mutateAll,DataFrame` <-  # nolint
    function(object, funs, ...) {
        object <- mutateAll(
            object = as_tibble(object, rownames = NULL),
            funs = funs,
            ...
        )
        out <- as(object, "DataFrame")
        rownames(out) <- rownames(object)
        out
    }



#' @rdname mutate
#' @export
setMethod(
    f = "mutateAll",
    signature = signature("DataFrame"),
    definition = `mutateAll,DataFrame`
)



`mutateAt,DataFrame` <-  # nolint
    function(object, vars, funs, ...) {
        ## FIXME
        stop("REWORK")
    }



#' @rdname mutate
#' @export
setMethod(
    f = "mutateAt",
    signature = signature("DataFrame"),
    definition = `mutateAt,DataFrame`
)



`mutateIf,DataFrame` <-  # nolint
    function(object, predicate, funs, ...) {
        ## FIXME
        stop("REWORK")
    }



#' @rdname mutate
#' @export
setMethod(
    f = "mutateIf",
    signature = signature("DataFrame"),
    definition = `mutateIf,DataFrame`
)



`transmuteAt,DataFrame` <-  # nolint
    function(object, vars, funs, ...) {
        ## FIXME
        stop("REWORK")
    }



#' @rdname mutate
#' @export
setMethod(
    f = "transmuteAt",
    signature = signature("DataFrame"),
    definition = `transmuteAt,DataFrame`
)



`transmuteIf,DataFrame` <-  # nolint
    function(object, predicate, funs, ...) {
        ## FIXME
        stop("REWORK")
    }



#' @rdname mutate
#' @export
setMethod(
    f = "transmuteIf",
    signature = signature("DataFrame"),
    definition = `transmuteIf,DataFrame`
)
