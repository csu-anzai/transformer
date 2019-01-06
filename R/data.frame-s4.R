#' Additional S4 `as.data.frame()` methods
#'
#' @name as.data.frame
#'
#' @examples
#' ## sparseMatrix ====
#' i <- c(1, 3:8)
#' j <- c(2, 9, 6:10)
#' x <- 7 * (1:7)
#' sm <- Matrix::sparseMatrix(i = i, j = j, x = x)
#' dim(sm)
#' summary(sm)
#' str(sm)
#' ## Now you can avoid having to coerce with `as.matrix()` first.
#' df <- as.data.frame(sm)
NULL



#' @importFrom BiocGenerics as.data.frame
#' @aliases NULL
#' @export
BiocGenerics::as.data.frame



#' @rdname as.data.frame
#' @export
setMethod(
    f = "as.data.frame",
    signature = signature("sparseMatrix"),
    definition = function(x, ...) {
        as.data.frame(as.matrix(x), ...)
    }
)



#' @rdname as
#' @name coerce,sparseMatrix,data.frame-method
setAs(
    from = "sparseMatrix",
    to = "data.frame",
    def = function(from) {
        as.data.frame(from)
    }
)
