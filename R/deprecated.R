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



## v0.2.4 =======================================================================
#' @rdname defunct
#' @name relevel
#' @importFrom stats relevel
#' @usage NULL
#' @export
NULL

#' @rdname defunct
#' @export
relevel.DataFrame <-
    function(x, ref = NULL, ...) {
        .Defunct("droplevels")
    }

#' @rdname defunct
#' @export
relevel.Ranges <- relevel.DataFrame

#' @rdname defunct
#' @export
relevel.SummarizedExperiment <- relevel.DataFrame



## nolint end
## nocov end
