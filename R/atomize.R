#' Atomize
#'
#' @note Input object must be coercible to `data.frame`.
#'
#' @export
#' @inheritParams params
#'
#' @return Modified object.
#' Non-atomic columns (e.g. `list` or complex S4 classes) will be dropped.
#'
#' @examples
#' df <- S4Vectors::DataFrame(a = "a", b = list(a = seq_len(3)))
#' lapply(df, is.atomic)
#' x <- atomize(df)
atomize <- function(object) {
    class <- class(object)[[1L]]
    # First, coerce to S3 data frame.
    # This step helps coerce nested S4 data to atomic columns.
    # This will also decode Rle columns.
    df <- as.data.frame(object)
    # Keep only atomic columns. Complex columns won't write to disk as CSVs
    # or work with R Markdown functions.
    keep <- vapply(X = df, FUN = is.atomic, FUN.VALUE = logical(1L))
    df <- df[, keep, drop = FALSE]
    assert(hasLength(df))
    as(df, Class = class)
}
