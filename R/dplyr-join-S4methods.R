#' Join operations supporting S4 class objects
#'
#' @rdname join
#' @name join
#' @note Updated 2019-08-14.
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
#' DataFrame <- S4Vectors::DataFrame
#'
#' ## DataFrame ====
#' df1 <- DataFrame(
#'     id = as.factor(seq(4L)),
#'     genotype = as.factor(rep(x = c("wt", "ko"), each = 2L))
#' )
#' print(df1)
#' df2 <- DataFrame(
#'     id = as.factor(seq(4L)),
#'     treatment = as.factor(rep(x = c("control", "expt"), times = 2L))
#' )
#' print(df2)
#' left_join(df1, df2, by = "id")
NULL



#' @rdname join
#' @export
setGeneric(
    name = "left_join",
    def = function(x, y, ...) {
        standardGeneric("left_join")
    }
)



## dplyr join functions, for reference:
## - `inner_join`
## - `left_join`
## - `right_join`
## - `full_join`
## - `semi_join`
## - `nest_join`
## - `anti_join`



## Updated 2019-08-14.
`left_join,data.frame` <-  # nolint
    function(x, y, ...) {
        requireNamespace("dplyr", quietly = TRUE)
        dplyr::left_join(x = x, y = y, ...)
    }



#' @rdname join
#' @export
setMethod(
    f = "left_join",
    signature = signature("data.frame"),
    definition = `left_join,data.frame`
)



## Currently setting an internal `.idx` column that we can use to reorder the
## rows after `merge()` operation.
##
## Can consider using Hervé Pagès's recommended approach instead.
## https://support.bioconductor.org/p/120277/
##
## Updated 2019-08-14.
`left_join,DataFrame` <-  # nolint
    function(
        x,
        y,
        by,
        copy = FALSE,
        suffix = NULL,
        ...
    ) {
        assert(
            is(x, "DataFrame"),
            is(y, "DataFrame"),
            isCharacter(by),
            areDisjointSets(".idx", colnames(x)),
            identical(copy, FALSE),
            is.null(suffix),
            !hasLength(list(...))
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
    signature = signature("DataFrame"),
    definition = `left_join,DataFrame`
)
