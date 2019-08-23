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
#' @section `data.frame` methods:
#'
#' Since we are defining S4 methods in this package, we are providing
#' passthrough support to dplyr for `data.frame` class objects.
#'
#' Refer to `help(topic = "join", package = "dplyr")` for details.
#'
#' @name join
#' @note Updated 2019-08-15.
#'
#' @param x,y `DataFrame`.
#'   Data frames, or objects, to be coerced into a single object.
#' @param by `character`.
#'   Column names to use for merge operation.
#'   Note that this is always required, unlike the dplyr `tbl_df` method, where
#'   this is optional.
#' @param ... Additional arguments.
#'
#' @seealso
#' - `help(topic = "join", package = "dplyr")`.
#' - `help(topic = "merge", package = "S4Vectors")`.
#' - https://support.bioconductor.org/p/120277/
#'
#' @return Merge of `x` and `y` objects.
#'
#' @examples
#' data(band_members, band_instruments)
#'
#' ## DataFrame ====
#' x <- band_members
#' print(x)
#' y <- band_instruments
#' print(y)
#' by <- "name"
#' innerJoin(x = x, y = y, by = by)
#' leftJoin(x = x, y = y, by = by)
#' rightJoin(x = x, y = y, by = by)
#' fullJoin(x = x, y = y, by = by)
#' semiJoin(x = x, y = y, by = by)
#' antiJoin(x = x, y = y, by = by)
NULL



`innerJoin,DataFrame` <-  # nolint
    function(x, y, by) {
        assert(
            isCharacter(by),
            isSubset(by, colnames(x)),
            isSubset(by, colnames(y)),
            areDisjointSets(c(".idx", ".idy"), colnames(x)),
            areDisjointSets(c(".idx", ".idy"), colnames(y))
        )
        x[[".idx"]] <- seq_len(nrow(x))
        out <- merge(x = x, y = y, by = by, all = FALSE, sort = FALSE)
        out <- out[order(out[[".idx"]]), , drop = FALSE]
        if (hasRownames(x)) {
            rownames(out) <- rownames(x)[out[[".idx"]]]
        }
        out <- out[, setdiff(colnames(out), ".idx"), drop = FALSE]
        out
    }



#' @rdname join
#' @export
setMethod(
    f = "innerJoin",
    signature = signature(
        x = "DataFrame",
        y = "DataFrame"
    ),
    definition = `innerJoin,DataFrame`
)



`leftJoin,DataFrame` <-  # nolint
    function(x, y, by) {
        assert(
            isCharacter(by),
            isSubset(by, colnames(x)),
            isSubset(by, colnames(y)),
            areDisjointSets(c(".idx", ".idy"), colnames(x)),
            areDisjointSets(c(".idx", ".idy"), colnames(y))
        )
        x[[".idx"]] <- seq_len(nrow(x))
        out <- merge(x = x, y = y, by = by, all.x = TRUE, sort = FALSE)
        out <- out[order(out[[".idx"]]), , drop = FALSE]
        assert(identical(x[[".idx"]], out[[".idx"]]))
        if (hasRownames(x)) {
            rownames(out) <- rownames(x)
        }
        out <- out[, setdiff(colnames(out), ".idx"), drop = FALSE]
        out
    }



#' @rdname join
#' @export
setMethod(
    f = "leftJoin",
    signature = signature(
        x = "DataFrame",
        y = "DataFrame"
    ),
    definition = `leftJoin,DataFrame`
)



`rightJoin,DataFrame` <-  # nolint
    function(x, y, by) {
        leftJoin(x = y, y = x, by = by)
    }



#' @rdname join
#' @export
setMethod(
    f = "rightJoin",
    signature = signature(
        x = "DataFrame",
        y = "DataFrame"
    ),
    definition = `rightJoin,DataFrame`
)



`fullJoin,DataFrame` <-  # nolint
    function(x, y, by) {
        assert(
            isCharacter(by),
            isSubset(by, colnames(x)),
            isSubset(by, colnames(y)),
            areDisjointSets(c(".idx", ".idy"), colnames(x)),
            areDisjointSets(c(".idx", ".idy"), colnames(y))
        )
        x[[".idx"]] <- seq_len(nrow(x))
        y[[".idy"]] <- seq_len(nrow(y))
        out <- merge(x = x, y = y, by = by, all = TRUE, sort = FALSE)
        out <- out[order(out[[".idx"]], out[[".idy"]]), , drop = FALSE]
        if (hasRownames(x) && hasRownames(y)) {
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
    f = "fullJoin",
    signature = signature(
        x = "DataFrame",
        y = "DataFrame"
    ),
    definition = `fullJoin,DataFrame`
)



`semiJoin,DataFrame` <-  # nolint
    function(x, y, by) {
        assert(
            isCharacter(by),
            isSubset(by, colnames(x)),
            isSubset(by, colnames(y)),
            areDisjointSets(c(".idx", ".idy"), colnames(x)),
            areDisjointSets(c(".idx", ".idy"), colnames(y))
        )
        x[[".idx"]] <- seq_len(nrow(x))
        m <- merge(x = x, y = y, by = by, all = FALSE, sort = FALSE)
        which <- m[[".idx"]]
        out <- x[which, setdiff(colnames(x), ".idx"), drop = FALSE]
        out
    }



#' @rdname join
#' @export
setMethod(
    f = "semiJoin",
    signature = signature(
        x = "DataFrame",
        y = "DataFrame"
    ),
    definition = `semiJoin,DataFrame`
)



`antiJoin,DataFrame` <-  # nolint
    function(x, y, by) {
        assert(
            isCharacter(by),
            isSubset(by, colnames(x)),
            isSubset(by, colnames(y)),
            areDisjointSets(c(".idx", ".idy"), colnames(x)),
            areDisjointSets(c(".idx", ".idy"), colnames(y))
        )
        x[[".idx"]] <- seq_len(nrow(x))
        m <- merge(x = x, y = y, by = by, all = FALSE, sort = FALSE)
        which <- order(setdiff(x[[".idx"]], m[[".idx"]]))
        out <- x[which, setdiff(colnames(x), ".idx"), drop = FALSE]
        out
    }



#' @rdname join
#' @export
setMethod(
    f = "antiJoin",
    signature = signature(
        x = "DataFrame",
        y = "DataFrame"
    ),
    definition = `antiJoin,DataFrame`
)
