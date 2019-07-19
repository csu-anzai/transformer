#' Reorder factor levels
#'
#' @name relevel
#' @inheritParams params
#' @param ... Additional arguments
#'
#' @param ref `NULL`.
#'   Unsupported argument that is part of S3 generic.
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
#' @usage relevel(x, ref, ...)
#' @export
NULL



#' @rdname relevel
#' @export
# Updated 2019-07-19.
relevel.DataFrame <-  # nolint
    function(x, ref = NULL, ...) {
        assert(is.null(ref))
        DataFrame(
            lapply(
                X = x,
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
            row.names = rownames(x)
        )
    }



#' @rdname relevel
#' @export
# Updated 2019-07-19.
relevel.GRanges <-  # nolint
    function(x, ref = NULL, ...) {
        assert(is.null(ref))
        mcols(x) <- relevel(mcols(x))
        x
    }
