#' @name factorize
#' @inherit bioverbs::factorize
#'
#' @inheritParams acidroxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' df <- S4Vectors::DataFrame(a = letters[seq_len(5L)], b = seq_len(5L))
#' x <- factorize(df)
NULL



#' @rdname factorize
#' @name factorize
#' @importFrom bioverbs factorize
#' @usage factorize(object, ...)
#' @export
NULL



## Updated 2019-07-19.
`factorize,DataFrame` <-  # nolint
    function(object) {
        class <- class(object)[[1L]]
        out <- lapply(
            X = object,
            FUN = function(x) {
                droplevels(as.factor(x))
            }
        )
        out <- as(out, Class = class)
        names(out) <- names(object)
        rownames <- rownames(object)
        if (!is.null(rownames)) {
            rownames(out) <- rownames
        }
        out
    }

#' @rdname factorize
#' @export
setMethod(
    f = "factorize",
    signature = signature("DataFrame"),
    definition = `factorize,DataFrame`
)
