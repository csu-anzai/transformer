#' @importClassesFrom GenomicRanges GenomicRanges
#' @importClassesFrom IRanges IPosRanges Ranges
#' @importClassesFrom Matrix Matrix
#' @importClassesFrom S4Vectors DataFrame Vector
#' @importClassesFrom SummarizedExperiment RangedSummarizedExperiment
#'   SummarizedExperiment
#' @importClassesFrom tibble tbl_df
#'
#' @importFrom IRanges end start width
#' @importFrom S4Vectors DataFrame Rle as.data.frame cbind decode do.call mcols
#'   mcols<- merge metadata metadata<- na.omit
#' @importFrom SummarizedExperiment colData colData<- rowData rowData<-
#'   rowRanges
#' @importFrom goalie allAreAtomic areDisjointSets assert bapply hasCols
#'   hasLength hasNames hasRows hasRownames isAny isCharacter isFlag isInt
#'   isSubset printString validNames
#' @importFrom methods as getMethod is setAs setMethod signature slotNames
#'   validObject .hasSlot
#' @importFrom tibble as_tibble
NULL



## This is needed to properly declare S4 `as()` coercion methods.
#' @importFrom methods coerce
#' @exportMethod coerce
NULL
