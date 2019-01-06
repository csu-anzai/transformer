#' Factorize
#'
#' @export
#' @inheritParams params
#'
#' @seealso `encode()` for Rle approach.
#'
#' @return Modified object.
#' All columns will be coerced to `factor`.
#'
#' @examples
#' df <- S4Vectors::DataFrame(a = letters[seq_len(5)], b = seq_len(5))
#' x <- factorize(df)
factorize <- function(object) {
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
