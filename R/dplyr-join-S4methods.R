#' Join operations supporting S4 class objects
#'
#' @section Ordering:
#'
#' The join functions never rearrange rows. To accomplish this, we're currently
#' setting an internal `.idx` column that we can use to reorder the rows after
#' [`merge()`][base::merge] operation.
#'
#' @section Row names:
#'
#' Unlike the S3 methods defined in dplyr, the join methods defined here for
#' `DataFrame` always preserve row names.
#'
#' @rdname join
#' @name join
#' @note Updated 2019-08-15.
#'
#' @param x,y `DataFrame`.
#'   Data frames, or objects, to be coerced into a single object.
#' @param by `character`.
#'   Column names to use for merge operation.
#'   Note that this is always required, unlike the dplyr `tbl_df` method, where
#'   this is optional.
#' @param copy,suffix,... Disabled for `DataFrame` method.
#'
#' @seealso
#' - `help(topic = "join", package = "dplyr")`.
#' - `help(topic = "merge", package = "S4Vectors")`.
#' - https://support.bioconductor.org/p/120277/
#'
#' @examples
#' data(band_members, band_instruments)
#' left_join(band_members, band_instruments, by = "name")
NULL



## data.frame ==================================================================
`inner_join,data.frame` <-  # nolint
    function(x, y, ...) {
        requireNamespace("dplyr", quietly = TRUE)
        dplyr::inner_join(x = x, y = y, ...)
    }



#' @rdname join
#' @export
setMethod(
    f = "inner_join",
    signature = signature(
        x = "data.frame",
        y = "data.frame"
    ),
    definition = `inner_join,data.frame`
)



`left_join,data.frame` <-  # nolint
    function(x, y, ...) {
        requireNamespace("dplyr", quietly = TRUE)
        dplyr::left_join(x = x, y = y, ...)
    }



#' @rdname join
#' @export
setMethod(
    f = "left_join",
    signature = signature(
        x = "data.frame",
        y = "data.frame"
    ),
    definition = `left_join,data.frame`
)



`right_join,data.frame` <-  # nolint
    function(x, y, ...) {
        requireNamespace("dplyr", quietly = TRUE)
        dplyr::right_join(x = x, y = y, ...)
    }



#' @rdname join
#' @export
setMethod(
    f = "right_join",
    signature = signature(
        x = "data.frame",
        y = "data.frame"
    ),
    definition = `right_join,data.frame`
)



`full_join,data.frame` <-  # nolint
    function(x, y, ...) {
        requireNamespace("dplyr", quietly = TRUE)
        dplyr::full_join(x = x, y = y, ...)
    }



#' @rdname join
#' @export
setMethod(
    f = "full_join",
    signature = signature(
        x = "data.frame",
        y = "data.frame"
    ),
    definition = `full_join,data.frame`
)



`semi_join,data.frame` <-  # nolint
    function(x, y, ...) {
        requireNamespace("dplyr", quietly = TRUE)
        dplyr::semi_join(x = x, y = y, ...)
    }



#' @rdname join
#' @export
setMethod(
    f = "semi_join",
    signature = signature(
        x = "data.frame",
        y = "data.frame"
    ),
    definition = `semi_join,data.frame`
)



`nest_join,data.frame` <-  # nolint
    function(x, y, ...) {
        requireNamespace("dplyr", quietly = TRUE)
        dplyr::nest_join(x = x, y = y, ...)
    }



#' @rdname join
#' @export
setMethod(
    f = "nest_join",
    signature = signature(
        x = "data.frame",
        y = "data.frame"
    ),
    definition = `nest_join,data.frame`
)



`anti_join,data.frame` <-  # nolint
    function(x, y, ...) {
        requireNamespace("dplyr", quietly = TRUE)
        dplyr::anti_join(x = x, y = y, ...)
    }



#' @rdname join
#' @export
setMethod(
    f = "anti_join",
    signature = signature(
        x = "data.frame",
        y = "data.frame"
    ),
    definition = `anti_join,data.frame`
)



## DataFrame ===================================================================
`inner_join,DataFrame` <-  # nolint
    function(x, y, by) {
        assert(
            isCharacter(by),
            isSubset(by, colnames(x)),
            isSubset(by, colnames(y)),
            areDisjointSets(".idx", colnames(x)),
            areDisjointSets(".idx", colnames(y))
        )
        x[[".idx"]] <- seq_len(nrow(x))
        out <- merge(x = x, y = y, by = by, all = FALSE, sort = FALSE)
        out <- out[order(out[[".idx"]]), , drop = FALSE]
        out <- out[, setdiff(colnames(out), ".idx"), drop = FALSE]
        rownames(out) <- rownames(x)
        out
    }



#' @rdname join
#' @export
setMethod(
    f = "inner_join",
    signature = signature(
        x = "DataFrame",
        y = "DataFrame"
    ),
    definition = `inner_join,DataFrame`
)



`left_join,DataFrame` <-  # nolint
    function(x, y, by) {
        assert(
            isCharacter(by),
            isSubset(by, colnames(x)),
            isSubset(by, colnames(y)),
            areDisjointSets(".idx", colnames(x)),
            areDisjointSets(".idx", colnames(y))
        )
        x[[".idx"]] <- seq_len(nrow(x))
        out <- merge(x = x, y = y, by = by, all.x = TRUE, sort = FALSE)
        out <- out[order(out[[".idx"]]), , drop = FALSE]
        assert(identical(x[[".idx"]], out[[".idx"]]))
        out <- out[, setdiff(colnames(out), ".idx"), drop = FALSE]
        rownames(out) <- rownames(x)
        out
    }



#' @rdname join
#' @export
setMethod(
    f = "left_join",
    signature = signature(
        x = "DataFrame",
        y = "DataFrame"
    ),
    definition = `left_join,DataFrame`
)



`right_join,DataFrame` <-  # nolint
    function(x, y, by) {
        left_join(x = y, y = x, by = by)
    }



#' @rdname join
#' @export
setMethod(
    f = "right_join",
    signature = signature(
        x = "DataFrame",
        y = "DataFrame"
    ),
    definition = `right_join,DataFrame`
)



`full_join,DataFrame` <-  # nolint
    function(x, y, by) {
        assert(
            isCharacter(by),
            isSubset(by, colnames(x)),
            isSubset(by, colnames(y)),
            areDisjointSets(".idx", colnames(x)),
            areDisjointSets(".idx", colnames(y))
        )
        x[[".idx"]] <- seq_len(nrow(x))
        out <- merge(x = x, y = y, by = by, all = TRUE, sort = FALSE)
        out <- out[order(out[[".idx"]]), , drop = FALSE]
        out <- out[, setdiff(colnames(out), ".idx"), drop = FALSE]
        rownames(out) <- rownames(x)
        out
    }



#' @rdname join
#' @export
setMethod(
    f = "full_join",
    signature = signature(
        x = "DataFrame",
        y = "DataFrame"
    ),
    definition = `full_join,DataFrame`
)



## FIXME semi
## FIXME nest
## FIXME anti
