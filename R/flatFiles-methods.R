# Consider soft deprecating in favor of `coerceS4ToList()`.



#' @name flatFiles
#' @inherit bioverbs::flatFiles
#'
#' @seealso [coerceS4ToList()].
#'
#' @return `list`.
#'
#' @examples
#' data(rse, package = "acidtest")
#'
#' ## SummarizedExperiment ====
#' x <- flatFiles(rse)
#' class(x)
#' names(x)
NULL



#' @rdname flatFiles
#' @name flatFiles
#' @importFrom bioverbs flatFiles
#' @export
NULL



flatFiles.SummarizedExperiment <-  # nolint
    function(object) {
        coerceS4ToList(object)
}



#' @rdname flatFiles
#' @export
setMethod(
    f = "flatFiles",
    signature = signature("SummarizedExperiment"),
    definition = flatFiles.SummarizedExperiment
)
