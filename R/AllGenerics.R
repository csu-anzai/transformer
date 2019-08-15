#' @rdname arrange
#' @export
setGeneric(
    name = "arrange_all",
    def = function(.tbl, ...) {
        standardGeneric("arrange_all")
    }
)

#' @rdname arrange
#' @export
setGeneric(
    name = "arrange_at",
    def = function(.tbl, ...) {
        standardGeneric("arrange_at")
    }
)

#' @rdname arrange
#' @export
setGeneric(
    name = "arrange_if",
    def = function(.tbl, ...) {
        standardGeneric("arrange_if")
    }
)



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




#' @rdname rename
#' @export
setGeneric(
    name = "rename_all",
    def = function(.tbl, ...) {
        standardGeneric("rename_all")
    }
)

#' @rdname rename
#' @export
setGeneric(
    name = "rename_at",
    def = function(.tbl, ...) {
        standardGeneric("rename_at")
    }
)

#' @rdname rename
#' @export
setGeneric(
    name = "rename_if",
    def = function(.tbl, ...) {
        standardGeneric("rename_if")
    }
)



#' @rdname select
#' @export
setGeneric(
    name = "select_all",
    def = function(.tbl, ...) {
        standardGeneric("select_all")
    }
)

#' @rdname select
#' @export
setGeneric(
    name = "select_at",
    def = function(.tbl, ...) {
        standardGeneric("select_at")
    }
)

#' @rdname select
#' @export
setGeneric(
    name = "select_if",
    def = function(.tbl, ...) {
        standardGeneric("select_if")
    }
)
