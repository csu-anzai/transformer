# This is needed to properly declare S4 `as()` coercion methods.
#' @importFrom methods coerce
#' @exportMethod coerce
NULL



#' @importFrom S4Vectors DataFrame
#' @export
S4Vectors::DataFrame



#' @importFrom SummarizedExperiment colData
#' @export
SummarizedExperiment::colData

#' @importFrom SummarizedExperiment rowRanges
#' @export
SummarizedExperiment::rowRanges
