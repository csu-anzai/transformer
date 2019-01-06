#' Force an object to belong to a class
#'
#' @name as
#' @aliases coerce
#' @inherit methods::as description references
#' @importFrom methods coerce
#' @exportMethod coerce
#'
#' @section list:
#'
#' It is often useful to coerce an S4 object to a flat `list` for archival
#' storage. Here we are providing the [coerceS4ToList()] function, which
#' consistently coerces the slots in any S4 to a standard `list`. Additionally,
#' here we have improved support for `SummarizedExperiment` to `list` coercion,
#' returning the slots as a `list`.
#'
#' @section tibble:
#'
#' Coerce an object to a [tibble][] (`tbl_df`, `data.frame`) using either S3 or
#' S4 methods:
#'
#' - S3: `as_tibble(x)` (or `as_tibble` alias).
#' - S4: `as(object, Class = "tbl_df")`.
#'
#' Tibbles don't support row name assignment, so here we are ensuring they are
#' kept by moving them to a column named `rowname` by default. This helps avoid
#' downstream unexpected data loss when using the [dplyr][] chain of single
#' table verbs, such as [`arrange()`][dplyr::arrange],
#' [`filter()`][dplyr::filter], or [`mutate()`][dplyr::mutate].
#'
#' This behavior can be overriden in the S3 method by setting `rowname = NULL`
#' instead, which is the current default in the tibble package. The S4 coercion
#' method doesn't support arguments, and therefore always attempts to move
#' rownames automatically, if defined.
#'
#' Conversely, when coercing a [tibble][] back to an S4 `DataFrame`, our [as()]
#' method looks for the `rowname` column and will attempt to move it back as
#' row names automatically, unless there are duplicates present.
#'
#' [dplyr]: https://dplyr.tidyverse.org/
#' [tibble]: https://tibble.tidyverse.org/
#'
#' @inheritParams params
#'
#' @return Object of new class.
#'
#' @seealso
#' - `methods::as()`.
#' - `methods::canCoerce()`.
#' - `methods::showMethods()`.
#' - `tibble::tibble()`.
#' - `utils::methods()`.
#'
#' @examples
#' data(rse)
#'
#' ## DataFrame to tbl_df ====
#' data <- SummarizedExperiment::colData(rse)
#' print(data)
#'
#' x <- as(data, "tbl_df")
#' print(x)
#'
#' x <- tibble::as_tibble(data)
#' print(x)
#'
#' ## GRanges to tbl_df ====
#' data <- SummarizedExperiment::rowRanges(rse)
#'
#' x <- as(data, "tbl_df")
#'
#' x <- tibble::as_tibble(data)
#' colnames(x)
#'
#' ## sparseMatrix to data.frame ====
#' data(sparse)
#'
#' x <- BiocGenerics::as.data.frame(sparse)
#' class(x)
#'
#' x <- as(sparse, "data.frame")
#' class(x)
#'
#' ## tbl_df to DataFrame ====
#' data <- tibble::as_tibble(datasets::iris)
#' x <- as(data, "DataFrame")
NULL
