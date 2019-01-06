#' S4Transformer
#'
#' Improved Bioconductor S4 coercion method support for tibble and sparse matrix
#' data classes.
#'
#' @importClassesFrom GenomicRanges GRanges
#' @importClassesFrom Matrix sparseMatrix
#' @importClassesFrom S4Vectors DataFrame
#' @importClassesFrom SummarizedExperiment SummarizedExperiment
#' @importClassesFrom tibble tbl_df
#'
#' @importFrom BiocGenerics as.data.frame
#' @importFrom S4Vectors metadata metadata<-
#' @importFrom SummarizedExperiment rowData rowData<- rowRanges
#' @importFrom goalie assert hasLength hasRownames
#' @importFrom methods as is setAs setMethod signature slotNames .hasSlot
#' @importFrom tibble as_tibble
"_PACKAGE"

# This is needed to properly declare S4 `as()` coercion methods.
#' @importFrom methods coerce
#' @exportMethod coerce
NULL
