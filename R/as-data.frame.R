#' Coerce to `data.frame`
#'
#' @name as-data.frame
#' @inheritParams base::as.data.frame
#'
#' @examples
#' ## sparseMatrix to data.frame ====
#' load(system.file("extdata", "sparseMatrix.rda", package = "transformer"))
#' x <- as(sparseMatrix, "data.frame")
#' head(x)
NULL



#' @rdname as-data.frame
#' @name as.data.frame
#' @importFrom BiocGenerics as.data.frame
#' @export
NULL



# S3(ish) ======================================================================
#' @rdname as-data.frame
#' @export
setMethod(
    f = "as.data.frame",
    signature = signature("sparseMatrix"),
    definition = function(x, ...) {
        as.data.frame(as.matrix(x), ...)
    }
)



# S4 ===========================================================================
#' @rdname as-data.frame
#' @name coerce,sparseMatrix,data.frame-method
setAs(
    from = "sparseMatrix",
    to = "data.frame",
    def = function(from) {
        as.data.frame(from)
    }
)
