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



# @seealso
# - `base:::as.data.frame.matrix()`.
# - `S4Vectors:::as.data.frame.DataTable()`.
atomize.ANY <-  # nolint
    function(object) {
        class <- class(object)[[1L]]
        # Note that going straight to data.frame using `as.data.frame()` doesn't
        # handle stringsAsFactors correctly for Rle, at least in BioC 3.7.
        df <- as(object, "DataFrame")
        # This step will convert Rle columns (e.g. in GRanges mcols).
        df <- decode(df)
        # Including this step here to coerce complex S4 columns, like IRanges.
        df <- as.data.frame(df)
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
