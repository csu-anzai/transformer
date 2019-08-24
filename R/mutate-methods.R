#' Mutate multiple columns
#'
#' @name mutate
#' @note Updated 2019-08-24.
#'
#' @inheritParams acidroxygen::params
#' @param .predicate `function` or `logical`.
#'   A predicate function to be applied to the columns, or a logical vector
#'   matching the number of columns.
#' @param .fun `function`.
#'   Mutation function.
#' @param .vars `character`.
#'   Column names.
#' @param ... Passthrough arguments to function declared in `.fun` argument.
#'
#' @return Modified object.
#'
#' @seealso
#' - `help(topic = "mutate_all", package = "dplyr")`
#'
#' @examples
#' data(mtcars, package = "datasets")
#'
#' ## DataFrame ====
#' x <- as(mtcars, "DataFrame")
#' mutateAll(x, .fun = log, base = 2L)
#' mutateAt(x, .vars = c("mpg", "cyl"), .fun = log, base = 2L)
#' mutateIf(x, .predicate = is.double, .fun = as.integer)
#' transmuteAt(x, .vars = c("mpg", "cyl"), .fun = log, base = 2L)
#' transmuteIf(x, .predicate = is.double, .fun = as.integer)
NULL



## Updated 2019-08-24.
`mutateAll,DataFrame` <-  # nolint
    function(object, .fun, ...) {
        x <- lapply(X = object, FUN = .fun, ...)
        x <- DataFrame(x, row.names = rownames(object))
        x
    }



#' @rdname mutate
#' @export
setMethod(
    f = "mutateAll",
    signature = signature("DataFrame"),
    definition = `mutateAll,DataFrame`
)



## Updated 2019-08-24.
`mutateAt,DataFrame` <-  # nolint
    function(object, .vars, .fun, ...) {
        x <- transmuteAt(
            object = object,
            .vars = .vars,
            .fun = .fun,
            ...
        )
        y <- object[, setdiff(colnames(object), colnames(x)), drop = FALSE]
        out <- cbind(x, y)
        out <- out[, colnames(object), drop = FALSE]
        out
    }



#' @rdname mutate
#' @export
setMethod(
    f = "mutateAt",
    signature = signature("DataFrame"),
    definition = `mutateAt,DataFrame`
)



## Updated 2019-08-24.
`mutateIf,DataFrame` <-  # nolint
    function(object, .predicate, .fun, ...) {
        x <- transmuteIf(
            object = object,
            .predicate = .predicate,
            .fun = .fun,
            ...
        )
        y <- object[, setdiff(colnames(object), colnames(x)), drop = FALSE]
        out <- cbind(x, y)
        out <- out[, colnames(object), drop = FALSE]
        out
    }



#' @rdname mutate
#' @export
setMethod(
    f = "mutateIf",
    signature = signature("DataFrame"),
    definition = `mutateIf,DataFrame`
)



## Updated 2019-08-24.
`transmuteAt,DataFrame` <-  # nolint
    function(object, .vars, .fun, ...) {
        x <- object[, .vars, drop = FALSE]
        x <- mutateAll(x, .fun = .fun, ...)
        x
    }



#' @rdname mutate
#' @export
setMethod(
    f = "transmuteAt",
    signature = signature("DataFrame"),
    definition = `transmuteAt,DataFrame`
)



## Updated 2019-08-24.
`transmuteIf,DataFrame` <-  # nolint
    function(object, .predicate, .fun, ...) {
        x <- selectIf(object, .predicate = .predicate)
        x <- mutateAll(x, .fun = .fun, ...)
        x
    }



#' @rdname mutate
#' @export
setMethod(
    f = "transmuteIf",
    signature = signature("DataFrame"),
    definition = `transmuteIf,DataFrame`
)
