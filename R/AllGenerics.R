#' @rdname join
#' @export
setGeneric(
    name = "innerJoin",
    def = function(x, y, ...) {
        standardGeneric("innerJoin")
    }
)

#' @rdname join
#' @export
setGeneric(
    name = "leftJoin",
    def = function(x, y, ...) {
        standardGeneric("leftJoin")
    }
)

#' @rdname join
#' @export
setGeneric(
    name = "rightJoin",
    def = function(x, y, ...) {
        standardGeneric("rightJoin")
    }
)

#' @rdname join
#' @export
setGeneric(
    name = "fullJoin",
    def = function(x, y, ...) {
        standardGeneric("fullJoin")
    }
)

#' @rdname join
#' @export
setGeneric(
    name = "semiJoin",
    def = function(x, y, ...) {
        standardGeneric("semiJoin")
    }
)

#' @rdname join
#' @export
setGeneric(
    name = "antiJoin",
    def = function(x, y, ...) {
        standardGeneric("antiJoin")
    }
)



#' @rdname mutate
#' @export
setGeneric(
    name = "mutateAll",
    def = function(.tbl, ...) {
        standardGeneric("mutateAll")
    }
)

#' @rdname mutate
#' @export
setGeneric(
    name = "mutateAt",
    def = function(.tbl, ...) {
        standardGeneric("mutateAt")
    }
)

#' @rdname mutate
#' @export
setGeneric(
    name = "mutateIf",
    def = function(.tbl, ...) {
        standardGeneric("mutateIf")
    }
)

#' @rdname mutate
#' @export
setGeneric(
    name = "transmuteAt",
    def = function(.tbl, ...) {
        standardGeneric("transmuteAt")
    }
)

#' @rdname mutate
#' @export
setGeneric(
    name = "transmuteIf",
    def = function(.tbl, ...) {
        standardGeneric("transmuteIf")
    }
)



#' @rdname select
#' @export
setGeneric(
    name = "selectAll",
    def = function(.tbl, ...) {
        standardGeneric("selectAll")
    }
)

#' @rdname select
#' @export
setGeneric(
    name = "selectAt",
    def = function(.tbl, ...) {
        standardGeneric("selectAt")
    }
)

#' @rdname select
#' @export
setGeneric(
    name = "selectIf",
    def = function(.tbl, ...) {
        standardGeneric("selectIf")
    }
)
