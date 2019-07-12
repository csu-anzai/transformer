#' @importClassesFrom GenomicRanges GRanges
#' @importClassesFrom IRanges IRanges
#' @importClassesFrom Matrix sparseMatrix
#' @importClassesFrom S4Vectors DataFrame
#' @importClassesFrom SummarizedExperiment SummarizedExperiment
#' @importClassesFrom tibble tbl_df
#'
#' @importFrom IRanges end start width
#' @importFrom S4Vectors as.data.frame mcols merge metadata metadata<-
#' @importFrom SummarizedExperiment rowData rowData<- rowRanges
#' @importFrom goalie areDisjointSets assert hasColnames hasLength hasRownames
#'   isCharacter printString validNames
#' @importFrom methods as is setAs setMethod signature slotNames .hasSlot
#' @importFrom tibble as_tibble
NULL



# This is needed to properly declare S4 `as()` coercion methods.
#' @importFrom methods coerce
#' @exportMethod coerce
NULL
