#' Coerce to `SummarizedExperiment`
#'
#' Improved method for reliably coercing objects that extend
#' `RangedSummarizedExperiment` to a standard `SummarizedExperiment`, that
#' doesn't drop [`rowData()`][SummarizedExperiment::rowData].
#'
#' @name as-SummarizedExperiment
#'
#' @inheritParams params
#'
#' @examples
#' load(system.file("extdata", "rse.rda", package = "S4Transformer"))
#' x <- as.SummarizedExperiment(rse)
NULL



# S3 ===========================================================================
#' @rdname as-SummarizedExperiment
#' @export
as.SummarizedExperiment <-  # nolint
    function(x) {
        UseMethod("as.SummarizedExperiment")
    }



# Note that our method here keeps track of `rowData()` when coercing an object
# that extends RangedSummarizedExperiment to SummarizedExperiment. This bug
# needs to be fixed in the SummarizedExperiment package.
#
# getMethod(
#     f = "coerce",
#     signature = signature(
#         from = "RangedSummarizedExperiment",
#         to = "SummarizedExperiment"
#     )
# )

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
# This is already defined in SummarizedExperiment package, so avoid here.
