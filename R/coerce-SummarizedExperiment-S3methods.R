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
        from <- x
        rowMeta <- metadata(rowRanges(from))
        to <- as(from, "RangedSummarizedExperiment")
        to <- as(from, "SummarizedExperiment")
        metadata(rowData(to)) <- rowMeta
        to
    }



#' @rdname coerce-SummarizedExperiment
#' @export
as.SummarizedExperiment.SummarizedExperiment <-  # nolint
    function(x) {
        function(x) {
            from <- x
            rowMeta <- metadata(rowData(from))
            to <- as(from, "SummarizedExperiment")
            metadata(rowData(to)) <- rowMeta
            to
        }
    }
