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
#' load(system.file("extdata", "rse.rda", package = "transformer"))
#'
#' x <- coerceS4ToList(rse)
#' class(x)
#' names(x)
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
