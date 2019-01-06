#' Additional S3 `as_tibble()` methods
#'
#' @name as_tibble
#'
#' @details
#' S4Transform adds method support for these S4 classes:
#'
#' - `DataFrame`.
#' - `GRanges`.
NULL



#' @importFrom tibble as_tibble
#' @aliases NULL
#' @export
tibble::as_tibble



#' @method as_tibble DataFrame
#' @export
as_tibble.DataFrame <-  # nolint
    function(x, ..., rownames = "rowname") {
        # Coerce to standard data frame.
        x <- as(x, "data.frame")
        # Check for valid columns (atomic, list).
        valid <- vapply(
            X = x,
            FUN = function(x) {
                is.atomic(x) || is.list(x)
            },
            FUN.VALUE = logical(1L),
            USE.NAMES = TRUE
        )
        # Error if S4 columns are nested.
        if (!all(valid)) {
            invalid <- names(valid[!valid])
            stop(paste0(
                "tibble supports atomic and list columns.\n",
                "Invalid columns: ", toString(invalid)
            ), call. = FALSE)
        }
        if (!hasRownames(x)) {
            rownames <- NULL
        }
        do.call(
            what = as_tibble,
            args = list(
                x = x,
                ...,
                rownames = rownames
            )
        )
    }



# The default handling from data.frame isn't clean, so add this.
# Default method will warn: `Arguments in '...' ignored`.
#' @method as_tibble GRanges
#' @export
as_tibble.GRanges <-  # nolint
    function(x, ..., rownames = "rowname") {
        names <- names(x)
        x <- as(x, "data.frame")
        rownames(x) <- names
        if (!hasRownames(x)) {
            rownames <- NULL
        }
        do.call(what = as_tibble, args = list(x = x, ..., rownames = rownames))
    }
