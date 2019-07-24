#' @name flatFiles
#' @inherit bioverbs::flatFiles
#'
#' @seealso [coerceS4ToList()].
#'
#' @return `list`.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "acidtest")
#'
#' ## SummarizedExperiment ====
#' x <- flatFiles(RangedSummarizedExperiment)
#' class(x)
#' names(x)
NULL



#' @rdname flatFiles
#' @name flatFiles
#' @importFrom bioverbs flatFiles
#' @export
NULL



## Updated 2019-07-19.
`flatFiles,SummarizedExperiment` <-  # nolint
    function(object) {
        coerceS4ToList(object)
}



#' @rdname flatFiles
#' @export
setMethod(
    f = "flatFiles",
    signature = signature("SummarizedExperiment"),
    definition = `flatFiles,SummarizedExperiment`
)
