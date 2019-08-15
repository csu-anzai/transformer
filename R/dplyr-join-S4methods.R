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
#' inner_join(band_members, band_instruments, by = "name")
#' left_join(band_members, band_instruments, by = "name")
#' right_join(band_members, band_instruments, by = "name")
#' full_join(band_members, band_instruments, by = "name")
#' semi_join(band_members, band_instruments, by = "name")
#' anti_join(band_members, band_instruments, by = "name")
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
    function(x, y, by, rownames = TRUE) {
        assert(
            isCharacter(by),
            isSubset(by, colnames(x)),
            isSubset(by, colnames(y)),
            areDisjointSets(c(".idx", ".idy"), colnames(x)),
            areDisjointSets(c(".idx", ".idy"), colnames(y)),
            isFlag(rownames)
        )
        x[[".idx"]] <- seq_len(nrow(x))
        out <- merge(x = x, y = y, by = by, all = FALSE, sort = FALSE)
        out <- out[order(out[[".idx"]]), , drop = FALSE]
        if (isTRUE(rownames)) {
            rownames(out) <- rownames(x)[out[[".idx"]]]
        }
        out <- out[, setdiff(colnames(out), ".idx"), drop = FALSE]
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
    function(x, y, by, rownames = TRUE) {
        assert(
            isCharacter(by),
            isSubset(by, colnames(x)),
            isSubset(by, colnames(y)),
            areDisjointSets(c(".idx", ".idy"), colnames(x)),
            areDisjointSets(c(".idx", ".idy"), colnames(y)),
            isFlag(rownames)
        )
        x[[".idx"]] <- seq_len(nrow(x))
        out <- merge(x = x, y = y, by = by, all.x = TRUE, sort = FALSE)
        out <- out[order(out[[".idx"]]), , drop = FALSE]
        assert(identical(x[[".idx"]], out[[".idx"]]))
        if (isTRUE(rownames)) {
            rownames(out) <- rownames(x)
        }
        out <- out[, setdiff(colnames(out), ".idx"), drop = FALSE]
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
    function(x, y, by, rownames = TRUE) {
        left_join(x = y, y = x, by = by, rownames = rownames)
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
    function(x, y, by, rownames = TRUE) {
        assert(
            isCharacter(by),
            isSubset(by, colnames(x)),
            isSubset(by, colnames(y)),
            areDisjointSets(c(".idx", ".idy"), colnames(x)),
            areDisjointSets(c(".idx", ".idy"), colnames(y)),
            isFlag(rownames)
        )
        x[[".idx"]] <- seq_len(nrow(x))
        y[[".idy"]] <- seq_len(nrow(y))
        out <- merge(x = x, y = y, by = by, all = TRUE, sort = FALSE)
        out <- out[order(out[[".idx"]], out[[".idy"]]), , drop = FALSE]
        if (isTRUE(rownames)) {
            rnx <- rownames(x)[na.omit(out[[".idx"]])]
            rny <- rownames(y)[na.omit(out[[".idy"]])]
            rn <- unique(c(rnx, rny))
            assert(hasLength(rn, n = nrow(out)))
            rownames(out) <- rn
        }
        out <- out[, setdiff(colnames(out), c(".idx", ".idy")), drop = FALSE]
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



`semi_join,DataFrame` <-  # nolint
    function(x, y, by, rownames = TRUE) {
        assert(
            isCharacter(by),
            isSubset(by, colnames(x)),
            isSubset(by, colnames(y)),
            areDisjointSets(c(".idx", ".idy"), colnames(x)),
            areDisjointSets(c(".idx", ".idy"), colnames(y)),
            isFlag(rownames)
        )
        x[[".idx"]] <- seq_len(nrow(x))
        m <- merge(x = x, y = y, by = by, all = FALSE, sort = FALSE)
        which <- order(m[[".idx"]])
        out <- x[which, setdiff(colnames(x), ".idx"), drop = FALSE]
        if (!isTRUE(rownames)) {
            rownames(out) <- NULL
        }
        out
    }



#' @rdname join
#' @export
setMethod(
    f = "semi_join",
    signature = signature(
        x = "DataFrame",
        y = "DataFrame"
    ),
    definition = `semi_join,DataFrame`
)



`anti_join,DataFrame` <-  # nolint
    function(x, y, by, rownames = TRUE) {
        assert(
            isCharacter(by),
            isSubset(by, colnames(x)),
            isSubset(by, colnames(y)),
            areDisjointSets(c(".idx", ".idy"), colnames(x)),
            areDisjointSets(c(".idx", ".idy"), colnames(y)),
            isFlag(rownames)
        )
        x[[".idx"]] <- seq_len(nrow(x))
        m <- merge(x = x, y = y, by = by, all = FALSE, sort = FALSE)
        which <- order(setdiff(x[[".idx"]], m[[".idx"]]))
        out <- x[which, setdiff(colnames(x), ".idx"), drop = FALSE]
        if (!isTRUE(rownames)) {
            rownames(out) <- NULL
        }
        out
    }



#' @rdname join
#' @export
setMethod(
    f = "anti_join",
    signature = signature(
        x = "DataFrame",
        y = "DataFrame"
    ),
    definition = `anti_join,DataFrame`
)
