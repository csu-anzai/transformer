#' Reorder factor levels
#'
#' @name relevel
#' @inheritParams params
#'
#' @return Modified object.
#' Factor levels will be readjusted (i.e. superfluous levels are dropped).
#'
#' @examples
#' data(rse, package = "acidtest")
#'
#' ## DataFrame
#' df <- SummarizedExperiment::rowData(rse)
#' x <- relevel(df)
#' summary(x)
#'
#' ## GRanges
#' gr <- SummarizedExperiment::rowRanges(rse)
#' x <- relevel(gr)
#' summary(x)
NULL



#' @rdname relevel
#' @name relevel
#' @importFrom stats relevel
#' @export
NULL



#' @rdname relevel
#' @export
# Updated 2019-07-19.
relevel.DataFrame <- function(object) {
    DataFrame(
        lapply(
            X = object,
            FUN = function(x) {
                if (is.factor(x)) {
                    droplevels(x)
                } else if (is(x, "Rle")) {
                    x <- decode(x)
                    if (is.factor(x)) {
                        x <- droplevels(x)
                    }
                    Rle(x)
                } else {
                    I(x)
                }
            }
        ),
        row.names = rownames(object)
    )
}



#' @rdname relevel
#' @export
# Updated 2019-07-19.
relevel.GRanges <- function(object) {
    mcols(object) <- relevel(mcols(object))
    object
}
