#' Decode column data that uses run-length encoding
#'
#' @name decode
#' @inherit S4Vectors::decode description return
#'
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @return Modified object.
#' Columns will be decoded and no longer `Rle` class.
#'
#' @seealso [S4Vectors::decode()].
#'
#' @examples
#' data(rse, package = "acidtest")
#'
#' ## DataFrame ====
#' df <- SummarizedExperiment::rowData(rse)
#' lapply(df, class)
#' x <- decode(mcols)
#' lapply(x, class)
NULL



#' @rdname decode
#' @name decode
#' @importFrom S4Vectors decode
#' @usage decode(x, ...)
#' @export
NULL



# Updated 2019-07-19.
`decode,DataFrame` <-  # nolint
    function(x) {
        DataFrame(
            lapply(
                X = x,
                FUN = function(x) {
                    if (is(x, "Rle")) {
                        x <- decode(x)
                        if (is.factor(x)) {
                            x <- droplevels(x)
                        }
                        x
                    } else if (!is.atomic(x)) {
                        I(x)
                    } else {
                        x
                    }
                }
            ),
            row.names = rownames(x)
        )
    }



#' @rdname decode
#' @export
setMethod(
    f = "decode",
    signature = signature("DataFrame"),
    definition = `decode,DataFrame`
)



# Updated 2019-07-20.
`decode,Ranges` <-  # nolint
    function(x) {
        if (!is.null(mcols(x))) {
            mcols(x) <- decode(mcols(x))
        }
        x
    }



#' @rdname decode
#' @export
setMethod(
    f = "decode",
    signature = signature("Ranges"),
    definition = `decode,Ranges`
)
