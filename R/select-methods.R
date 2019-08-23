## FIXME Rework and rename the formals.
## FIXME How to support `funs()` here?



#' Select multiple columns
#'
#' @name select
#' @note Updated 2019-08-23.
#'
#' @inheritParams mutate
#' @param ... Passthrough arguments to `predicate` function.
#'
#' @return Modified object.
#'
#' @seealso
#' - `help(topic = "select_all", package = "dplyr")`
#'
#' @examples
#' data(iris, package = "datasets")
#'
#' ## DataFrame ====
#' x <- as(iris, "DataFrame")
#' selectIf(x, .predicate = is.factor)
NULL



`selectIf,DataFrame` <-  # nolint
    function(object, .predicate, ...) {
        assert(isAny(.predicate, c("function", "logical")))
        if (is.function(.predicate)) {
            keep <- bapply(X = object, FUN = .predicate, ...)
        } else {
            keep <- .predicate
        }
        object[, keep, drop = FALSE]
    }



#' @rdname select
#' @export
setMethod(
    f = "selectIf",
    signature = signature("DataFrame"),
    definition = `selectIf,DataFrame`
)
