#' Coerce to `data.frame`
#'
#' @name coerce-data.frame
#' @inheritParams base::as.data.frame
#'
#' @examples
#' ## sparseMatrix to data.frame ====
#' load(system.file("extdata", "sparseMatrix.rda", package = "transformer"))
#' x <- as(sparseMatrix, "data.frame")
#' head(x)
NULL



# S3(ish) ======================================================================
#' @rdname coerce-data.frame
#' @name as.data.frame
#' @importFrom BiocGenerics as.data.frame
#' @export
NULL



#' @rdname coerce-data.frame
#' @export
setMethod(
    f = "as.data.frame",
    signature = signature("sparseMatrix"),
    definition = function(x, ...) {
        as.data.frame(as.matrix(x), ...)
    }
)



# S4 ===========================================================================
#' @rdname coerce-data.frame
#' @name coerce,sparseMatrix,data.frame-method
setAs(
    from = "sparseMatrix",
    to = "data.frame",
    def = function(from) {
        as.data.frame(from)
    }
)
