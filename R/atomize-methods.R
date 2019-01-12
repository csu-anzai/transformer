#' @name atomize
#' @inherit bioverbs::atomize
#' @inheritParams params
#' @examples
#' df <- DataFrame(a = "a", b = list(a = seq_len(3)))
#' lapply(df, is.atomic)
#' x <- atomize(df)
NULL



#' @importFrom bioverbs atomize
#' @aliases NULL
#' @export
bioverbs::atomize



atomize.ANY <-
    function(object) {
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



#' @rdname atomize
#' @export
setMethod(
    f = "atomize",
    signature = signature("ANY"),
    definition = atomize.ANY
)
