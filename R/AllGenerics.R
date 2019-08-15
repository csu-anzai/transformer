#' @rdname join
#' @export
setGeneric(
    name = "inner_join",
    def = function(x, y, ...) {
        standardGeneric("inner_join")
    }
)

#' @rdname join
#' @export
setGeneric(
    name = "left_join",
    def = function(x, y, ...) {
        standardGeneric("left_join")
    }
)

#' @rdname join
#' @export
setGeneric(
    name = "right_join",
    def = function(x, y, ...) {
        standardGeneric("right_join")
    }
)

#' @rdname join
#' @export
setGeneric(
    name = "full_join",
    def = function(x, y, ...) {
        standardGeneric("full_join")
    }
)

#' @rdname join
#' @export
setGeneric(
    name = "semi_join",
    def = function(x, y, ...) {
        standardGeneric("semi_join")
    }
)

#' @rdname join
#' @export
setGeneric(
    name = "anti_join",
    def = function(x, y, ...) {
        standardGeneric("anti_join")
    }
)



#' @rdname mutate
#' @export
setGeneric(
    name = "mutate_all",
    def = function(.tbl, ...) {
        standardGeneric("mutate_all")
    }
)



#' @rdname mutate
#' @export
setGeneric(
    name = "mutate_at",
    def = function(.tbl, ...) {
        standardGeneric("mutate_at")
    }
)



#' @rdname mutate
#' @export
setGeneric(
    name = "mutate_if",
    def = function(.tbl, ...) {
        standardGeneric("mutate_if")
    }
)
