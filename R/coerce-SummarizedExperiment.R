#' Coerce to `SummarizedExperiment`
#'
#' Improved method for reliably coercing objects that extend
#' `RangedSummarizedExperiment` to a standard `SummarizedExperiment`, that
#' doesn't drop [`rowData()`][SummarizedExperiment::rowData].
#'
#' @name coerce-SummarizedExperiment
#'
#' @inheritParams params
#'
#' @examples
#' load(system.file("extdata", "rse.rda", package = "transformer"))
#' x <- as.SummarizedExperiment(rse)
NULL



# S3 ===========================================================================
#' @rdname coerce-SummarizedExperiment
#' @export
as.SummarizedExperiment <-  # nolint
    function(x) {
        UseMethod("as.SummarizedExperiment")
    }



# Note that our method here keeps track of `rowData()` when coercing an object
# that extends RangedSummarizedExperiment to SummarizedExperiment. This bug
# needs to be fixed in SummarizedExperiment.

#' @method as.SummarizedExperiment default
#' @export
as.SummarizedExperiment.default <-  # nolint
    function(x) {
        if (is(x, "RangedSummarizedExperiment")) {
            rowMeta <- metadata(rowRanges(x))
            x <- as(x, "RangedSummarizedExperiment")
        } else {
            rowMeta <- metadata(rowData(x))
        }
        x <- as(x, "SummarizedExperiment")
        metadata(rowData(x)) <- rowMeta
        x
    }



# S4 ===========================================================================
# S4 methods are already defined in SummarizedExperiment.
