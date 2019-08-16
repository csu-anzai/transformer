## nocov start
## nolint start



#' @name defunct
#' @inherit acidroxygen::defunct description examples return seealso title
#' @inheritParams acidroxygen::params
#' @keywords internal
NULL



#' @name deprecated
#' @inherit acidroxygen::deprecated description examples return seealso title
#' @inheritParams acidroxygen::params
#' @keywords internal
NULL



## v0.2.4 ======================================================================
#' @rdname deprecated
#' @name relevel
#' @importFrom stats relevel
#' @param ref Unsupported.
#' @usage relevel(x, ref, ...)
#' @export
NULL

#' @rdname deprecated
#' @export
relevel.DataFrame <-  # nolint
    function(x, ref = NULL, ...) {
        ## > .Deprecated("droplevels")  # nolint
        assert(is.null(ref))
        if (!hasRows(x) || !hasCols(x)) return(x)
        DataFrame(
            lapply(
                X = x,
                FUN = function(x) {
                    if (is.factor(x)) {
                        droplevels(x)
                    } else if (is(x, "Rle")) {
                        x <- decode(x)
                        if (is.factor(x)) {
                            x <- droplevels(x)
                        }
                        Rle(x)
                    } else {
                        I(x)
                    }
                }
            ),
            row.names = rownames(x)
        )
    }

#' @rdname deprecated
#' @export
relevel.Ranges <-  # nolint
    function(x, ref = NULL, ...) {
        ## > .Deprecated("droplevels")  # nolint
        assert(is.null(ref))
        if (!is.null(mcols(x))) {
            mcols <- mcols(x)
            mcols <- relevel(mcols)
            mcols(x) <- mcols
        }
        x
    }

#' @rdname deprecated
#' @export
relevel.SummarizedExperiment <-  # nolint
    function(x, ref = NULL, ...) {
        ## > .Deprecated("droplevels")  # nolint
        assert(is.null(ref))
        rowData(x) <- relevel(rowData(x))
        colData(x) <- relevel(colData(x))
        x
    }



## nolint end
## nocov end
