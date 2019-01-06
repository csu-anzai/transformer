as.DataFrame <-  # nolint
    function(x) {
        UseMethod("as.DataFrame")
    }



as.DataFrame.default <- function(x) {
    to <- as.data.frame(x, stringsAsFactors = FALSE)
    to <- as(to, "DataFrame")
    rownames <- as.character(to[["rowname"]])
    if (
        length(rownames) > 0L &&
        !any(duplicated(rownames))
    ) {
        rownames(to) <- rownames
        to[["rowname"]] <- NULL
    }
    to
}
