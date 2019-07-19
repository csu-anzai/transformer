#' Coerce to summarized experiment
#'
#' Coerce to `SummarizedExperiment`.
#'
#' Improved S3 methods for reliably coercing objects that extend
#' `RangedSummarizedExperiment` to a standard `SummarizedExperiment`, that
#' doesn't drop [`rowData()`][SummarizedExperiment::rowData].
#'
#' @name coerce-SummarizedExperiment
#'
#' @inheritParams params
#'
#' @examples
#' suppressPackageStartupMessages(library(SummarizedExperiment))
#' data(rse, package = "acidtest")
#'
#' ## RangedSummarizedExperiment ===
#' ## Current S4 coercion drops row metadata.
#' names(metadata(rowRanges(rse)))
#' se <- as(rse, "SummarizedExperiment")
#' names(metadata(rowData(se)))
#' ## Our alternate S3 method preserves the metadata.
#' se <- as.SummarizedExperiment(rse)
#' names(metadata(rowData(se)))
NULL



# S3 ===========================================================================
#' @rdname coerce-SummarizedExperiment
#' @export
as.SummarizedExperiment <-  # nolint
    function(x) {
        UseMethod("as.SummarizedExperiment")
    }



#' @rdname coerce-SummarizedExperiment
#' @export
as.SummarizedExperiment.RangedSummarizedExperiment <-  # nolint
    function(x) {
        `coerce,RangedSummarizedExperiment,SummarizedExperiment`(x)
    }



#' @rdname coerce-SummarizedExperiment
#' @export
as.SummarizedExperiment.SummarizedExperiment <-  # nolint
    function(x) {
        `coerce,SummarizedExperiment,SummarizedExperiment`(x)
    }



# S4 ===========================================================================
# Can't set S4 coercion methods because RSE and SE aren't local, modifiable.

# Coercion methods of interest:
# - from="RangedSummarizedExperiment", to="SummarizedExperiment"
# - from="SingleCellExperiment", to="RangedSummarizedExperiment"
# - from="SingleCellExperiment", to="SummarizedExperiment"



# Updated 2019-07-19.
`coerce,RangedSummarizedExperiment,SummarizedExperiment` <-  # nolint
    function(from) {
        rowMeta <- metadata(rowRanges(from))
        to <- as(from, "RangedSummarizedExperiment")
        to <- as(from, "SummarizedExperiment")
        metadata(rowData(to)) <- rowMeta
        to
    }



# Updated 2019-07-19.
`coerce,SummarizedExperiment,SummarizedExperiment` <-  # nolint
    function(from) {
        rowMeta <- metadata(rowData(from))
        to <- as(from, "SummarizedExperiment")
        metadata(rowData(to)) <- rowMeta
        to
    }
