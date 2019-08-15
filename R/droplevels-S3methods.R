#' Drop unused levels from factors
#'
#' @name droplevels
#' @note Additional useful methods for S4 objects are defined in S4Vectors.
#' @note Updated 2019-08-15.
#'
#' @inheritParams acidroxygen::params
#' @param ... Additional arguments.
#'
#' @return Modified object.
#' Factor levels will be readjusted (i.e. superfluous levels are dropped).
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "acidtest")
#'
#' ## DataFrame ====
#' object <- RangedSummarizedExperiment
#' object <- droplevels(object)
NULL



#' @rdname droplevels
#' @export
## Updated 2019-08-15.
droplevels.SummarizedExperiment <-  # nolint
    function(x, ...) {
        rowData(x) <- droplevels(rowData(x))
        colData(x) <- droplevels(colData(x))
        x
    }
