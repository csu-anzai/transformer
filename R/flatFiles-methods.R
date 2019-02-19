# Consider soft deprecating in favor of `coerceS4ToList()`.



#' @name flatFiles
#' @inherit bioverbs::flatFiles
#'
#' @seealso [coerceS4ToList()].
#'
#' @return `list`.
#'
#' @examples
#' load(system.file("extdata", "rse.rda", package = "transformer"))
#'
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
