#' Coerce to `data.frame`
#'
#' @name as-data.frame
#' @inheritParams base::as.data.frame
#'
#' @examples
#' ## sparseMatrix ====
#' load(system.file("extdata", "sparseMatrix.rda", package = "S4Transformer"))
#' x <- as(sparseMatrix, "data.frame")
#' head(x)
NULL



#' @rdname as-data.frame
#' @name as.data.frame
#' @importFrom BiocGenerics as.data.frame
#' @export
NULL



#' @rdname as-data.frame
#' @export
setMethod(
    f = "as.data.frame",
    signature = signature("sparseMatrix"),
    definition = function(x, ...) {
        as.data.frame(as.matrix(x), ...)
    }
)



#' @rdname as-data.frame
#' @name coerce,sparseMatrix,data.frame-method
setAs(
    from = "sparseMatrix",
    to = "data.frame",
    def = function(from) {
        as.data.frame(from)
    }
)
