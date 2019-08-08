#' Join operations supporting S4 class objects
#'
#' @rdname join
#' @name join
#'
#' @param x,y `DataFrame`.
#'   Data frames, or objects, to be coerced into a single object.
#' @param by `character`.
#'   Column names to use for merge operation.
#'   Note that this is always required, unlike the dplyr `tbl_df` method, where
#'   this is optional.
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



## dplyr join functions, for reference:
## - `inner_join`
## - `left_join`
## - `right_join`
## - `full_join`
## - `semi_join`
## - `nest_join`
## - `anti_join`

## Currently setting an internal `.idx` column that we can use to reorder the
## rows after `merge()` operation.

## Can consider using Hervé Pagès's recommended approach instead.
## https://support.bioconductor.org/p/120277/



#' @importFrom dplyr left_join
#' @export
dplyr::left_join



#' @rdname join
#' @export
## Updated 2019-08-07.
left_join.DataFrame <-  # nolint
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
        out
    }
