#' Coerce any S4 object to a list
#'
#' @export
#' @inheritParams methods::coerce
#' @inheritParams params
#'
#' @seealso `methods::coerce()`.
#'
#' @return `list`.
#'
#' @examples
#' data(rse, package = "acidtest")
#'
#' ## SummarizedExperiment ====
#' x <- coerceS4ToList(rse)
#' class(x)
#' names(x)

# Updated 2019-07-19.
coerceS4ToList <- function(from) {
    assert(isS4(from))
    to <- lapply(slotNames(from), function(slot) {
        if (.hasSlot(from, slot)) {
            slot(from, slot)
        } else {
            NULL  # nocov
        }
    })
    names(to) <- slotNames(from)
    to
}
