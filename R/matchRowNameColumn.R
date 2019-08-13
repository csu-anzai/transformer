#' Match row name column
#'
#' Automatically detect row names column, if defined.
#'
#' @export
#' @inheritParams acidroxygen::params
#'
#' @return `character(1)` or `NULL`.
#'
#'   - data.table: `"rn"`.
#'   - tibble: `"rowname"`.
#'
#' @examples
#' data(data.table, tbl_df, package = "acidtest")
#'
#' ## data.table ====
#' matchRowNameColumn(data.table)
#'
#' ## tbl_df ====
#' matchRowNameColumn(tbl_df)

## Updated 2019-07-19.
matchRowNameColumn <- function(object) {
    assert(!hasRownames(object))
    match <- na.omit(match(
        x = c(
            "rn",
            "row_name",
            "row_names",
            "row.name",
            "row.names",
            "rowname",
            "rowName",
            "rownames",
            "rowNames"
        ),
        table = colnames(object),
        nomatch = NA_integer_
    ))
    if (!hasLength(match)) {
        NULL
    } else if (length(match) == 1L) {
        col <- colnames(object)[[match]]
        rownames <- as.character(object[[col]])
        assert(validNames(rownames))
        col
    } else if (length(match) > 1L) {
        stop(sprintf(
            "Multiple row names columns detected: %s.",
            toString(colnames(object)[match], width = 100L)
        ))
    }
}
