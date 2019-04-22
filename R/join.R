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
#' - `dplyr::left_join()`.
#' - `S4Vectors::merge()`.
NULL



#' @rdname join
#' @export
left_join.DataFrame <-
    function(x, y, by) {
        assert(
            is(x, "DataFrame"),
            is(y, "DataFrame"),
            isCharacter(by),
            areDisjointSets(".idx", colnames(x))
        )
        # Setting internal `.idx` column here to avoid row reorders.
        x[[".idx"]] <- seq_len(nrow(x))
        out <- S4Vectors::merge(
            x = x, y = y, by = by,
            all.x = TRUE, sort = FALSE
        )
        # Now ensure original row order is preserved, using `.idx` values.
        out <- out[order(out[[".idx"]]), , drop = FALSE]
        assert(identical(x[[".idx"]], out[[".idx"]]))
        # Don't use `NULL` assignment on S4 columns.
        # This isn't backward compatible with BioC 3.6.
        out <- out[, setdiff(colnames(out), ".idx"), drop = FALSE]
        out
    }



# left_join
# right_join
# full_join
# semi_join
# nest_join
# anti_join
