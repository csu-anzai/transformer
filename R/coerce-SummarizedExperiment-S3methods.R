#' Coerce to summarized experiment
#'
#' Coerce to `SummarizedExperiment`.
#'
#' Improved S3 methods for reliably coercing objects that extend
#' `RangedSummarizedExperiment` to a standard `SummarizedExperiment`, that
#' doesn't drop [`rowData()`][SummarizedExperiment::rowData].
#'
#' We can't set S4 coercion methods here because RangedSummarizedExperiment and
#' SummarizedExperiment aren't local, modifiable.
#'
#' Related S4 coercion method of interest:
#'
#' ```
#' getMethod(
#'     f = "coerce",
#'     signature = signature(
#'         from = "RangedSummarizedExperiment",
#'         to = "SummarizedExperiment"
#'     ),
#'     where = asNamespace("SummarizedExperiment")
#' )
#' ```
#'
#' @name coerce-SummarizedExperiment
#'
#' @inheritParams params
#'
#' @examples
#' suppressPackageStartupMessages(library(SummarizedExperiment))
#' data(RangedSummarizedExperiment, package = "acidtest")
#'
#' ## RangedSummarizedExperiment ===
#' ## Current S4 coercion drops row metadata.
#' names(metadata(rowRanges(RangedSummarizedExperiment)))
#' se <- as(RangedSummarizedExperiment, "SummarizedExperiment")
#' names(metadata(rowData(se)))
#' ## Our alternate S3 method preserves the metadata.
#' se <- as.SummarizedExperiment(RangedSummarizedExperiment)
#' names(metadata(rowData(se)))
NULL



#' @rdname coerce-SummarizedExperiment
#' @export
as.SummarizedExperiment <-  # nolint
    function(x) {
        UseMethod("as.SummarizedExperiment")
    }



#' @rdname coerce-SummarizedExperiment
#' @export
## Updated 2019-07-22.
as.SummarizedExperiment.RangedSummarizedExperiment <-  # nolint
    function(x) {
        rowMeta <- metadata(rowRanges(x))
        y <- x
        y <- as(y, "RangedSummarizedExperiment")
        y <- as(y, "SummarizedExperiment")
        metadata(rowData(y)) <- rowMeta
        y
    }



#' @rdname coerce-SummarizedExperiment
#' @export
## Updated 2019-07-22.
as.SummarizedExperiment.SummarizedExperiment <-  # nolint
    function(x) {
        rowMeta <- metadata(rowData(x))
        y <- x
        y <- as(y, "SummarizedExperiment")
        metadata(rowData(y)) <- rowMeta
        y
    }
