#' Join operations supporting S4 class objects
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
#'
#' @examples
#' data(band_members, band_instruments)
#' left_join(band_members, band_instruments, by = "name")
NULL



## Generics ====================================================================
#' @rdname join
#' @export
setGeneric(
    name = "inner_join",
    def = function(x, y, ...) {
        standardGeneric("inner_join")
    }
)

#' @rdname join
#' @export
setGeneric(
    name = "left_join",
    def = function(x, y, ...) {
        standardGeneric("left_join")
    }
)

#' @rdname join
#' @export
setGeneric(
    name = "right_join",
    def = function(x, y, ...) {
        standardGeneric("right_join")
    }
)

#' @rdname join
#' @export
setGeneric(
    name = "full_join",
    def = function(x, y, ...) {
        standardGeneric("full_join")
    }
)

#' @rdname join
#' @export
setGeneric(
    name = "semi_join",
    def = function(x, y, ...) {
        standardGeneric("semi_join")
    }
)

#' @rdname join
#' @export
setGeneric(
    name = "nest_join",
    def = function(x, y, ...) {
        standardGeneric("nest_join")
    }
)

#' @rdname join
#' @export
setGeneric(
    name = "anti_join",
    def = function(x, y, ...) {
        standardGeneric("anti_join")
    }
)



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
## Currently setting an internal `.idx` column that we can use to reorder the
## rows after `merge()` operation.
##
## Can consider using Hervé Pagès's recommended approach instead.
## https://support.bioconductor.org/p/120277/



`inner_join,DataFrame` <-  # nolint
    function(x, y, by) {

    }



`left_join,DataFrame` <-  # nolint
    function(x, y, by) {
        assert(
            is(x, "DataFrame"),
            is(y, "DataFrame"),
            isCharacter(by),
            areDisjointSets(".idx", colnames(x))
        )
        ## Setting internal `.idx` column here to avoid row reorders.
        x[[".idx"]] <- seq_len(nrow(x))
        out <- merge(x = x, y = y, by = by, all.x = TRUE, sort = FALSE)
        ## Now ensure original row order is preserved, using `.idx` values.
        out <- out[order(out[[".idx"]]), , drop = FALSE]
        assert(identical(x[[".idx"]], out[[".idx"]]))
        ## Don't use `NULL` assignment on S4 columns.
        ## This isn't backward compatible with BioC 3.6.
        out <- out[, setdiff(colnames(out), ".idx"), drop = FALSE]
        ## Ensure rownames are preserved.
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



## FIXME full
## FIXME semi
## FIXME nest
## FIXME anti
