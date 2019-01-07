#' @rdname coerceS4ToList
#' @name flatFiles
#' @importFrom bioverbs flatFiles
#' @export
NULL



# Consider soft deprecating in favor of `coerceS4ToList`.
flatFiles.SummarizedExperiment <-  # nolint
    function(object) {
        coerceS4ToList(object)
}



#' @rdname coerceS4ToList
#' @export
setMethod(
    f = "flatFiles",
    signature = signature("SummarizedExperiment"),
    definition = flatFiles.SummarizedExperiment
)
