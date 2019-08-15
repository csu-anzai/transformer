#' @importClassesFrom GenomicRanges GenomicRanges
#' @importClassesFrom IRanges IPosRanges Ranges
#' @importClassesFrom Matrix Matrix
#' @importClassesFrom S4Vectors DataFrame Vector
#' @importClassesFrom SummarizedExperiment RangedSummarizedExperiment
#'   SummarizedExperiment
#' @importClassesFrom tibble tbl_df
#'
#' @importFrom IRanges end start width
#' @importFrom S4Vectors DataFrame Rle as.data.frame decode mcols mcols<- merge
#'   metadata metadata<-
#' @importFrom SummarizedExperiment colData colData<- rowData rowData<-
#'   rowRanges
#' @importFrom goalie areDisjointSets assert hasCols hasLength hasNames hasRows
#'   hasRownames isCharacter isFlag isSubset printString validNames
#' @importFrom methods as getMethod is setAs setMethod signature slotNames
#'   validObject .hasSlot
#' @importFrom stats na.omit
#' @importFrom tibble as_tibble
NULL



## This is needed to properly declare S4 `as()` coercion methods.
#' @importFrom methods coerce
#' @exportMethod coerce
NULL
