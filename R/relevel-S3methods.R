#' Reorder factor levels
#'
#' @name relevel
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @param ref `NULL`.
#'   Unsupported argument that is part of S3 generic.
#'
#' @return Modified object.
#' Factor levels will be readjusted (i.e. superfluous levels are dropped).
#'
#' @examples
#' library(SummarizedExperiment)
#' data(RangedSummarizedExperiment, package = "acidtest")
#'
#' ## DataFrame ====
#' df <- rowData(RangedSummarizedExperiment)
#' x <- relevel(df)
#' summary(x)
#'
#' ## GenomicRanges ====
#' gr <- rowRanges(RangedSummarizedExperiment)
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
## Updated 2019-07-19.
relevel.DataFrame <-  # nolint
    function(x, ref = NULL, ...) {
        assert(is.null(ref))
        if (!hasRows(x)) return(x)
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


## Note that GenomicRanges and IRanges extend this virtual class. Releveling the
## factors inside of `mcols()` is particularly useful when subsetting large
## genomic ranges, reducing the memory overhead associated with storing
## irrelevant factor levels (e.g. gene metadata no longer in object).

#' @rdname relevel
#' @export
## Updated 2019-07-20.
relevel.Ranges <-  # nolint
    function(x, ref = NULL, ...) {
        assert(is.null(ref))
        if (!is.null(mcols(x))) {
            mcols(x) <- relevel(mcols(x))
        }
        x
    }



#' @rdname relevel
#' @export
## Updated 2019-07-20.
relevel.SummarizedExperiment <-  # nolint
    function(x, ref = NULL, ...) {
        assert(is.null(ref))
        rowData(x) <- relevel(rowData(x))
        colData(x) <- relevel(colData(x))
        x
    }
