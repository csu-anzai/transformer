#' Coerce to summarized experiment
#'
#' Coerce to `SummarizedExperiment`.
#'
#' Improved S3 methods for reliably coercing objects that extend
#' `RangedSummarizedExperiment` to a standard `SummarizedExperiment`, that
#' doesn't drop [`rowData()`][SummarizedExperiment::rowData].
#'
#' @name coerce-SummarizedExperiment
#'
#' @inheritParams params
#'
#' @examples
#' data(rse, sce, package = "acidtest")
#'
#' ## RangedSummarizedExperiment ===
#' ## Current S4 coercion drops row metadata.
#' names(metadata(rowRanges(rse)))
#' se <- as(rse, "SummarizedExperiment")
#' names(metadata(rowData(se)))
#' ## Our alternate S3 method preserves the metadata.
#' se <- as.SummarizedExperiment(rse)
#' names(metadata(rowData(se)))
#'
#' ## Extends RangedSummarizedExperiment ====
#' is(sce, "RangedSummarizedExperiment")
#' ## Current S4 coercion drops row data.
#' se <- as(sce, "SummarizedExperiment")
#' rowData(se)
#' ## Our alternate S3 method preserves the row data.
#' se <- as.SummarizedExperiment(sce)
#' rowData(se)
NULL



# S3 ===========================================================================
#' @rdname coerce-SummarizedExperiment
#' @export
as.SummarizedExperiment <-  # nolint
    function(x) {
        UseMethod("as.SummarizedExperiment")
    }



#' @rdname coerce-SummarizedExperiment
#' @export
as.SummarizedExperiment.RangedSummarizedExperiment <-  # nolint
    function(x) {
        `.coerce.RangedSummarizedExperiment,SummarizedExperiment`(x)
    }



# S4 ===========================================================================
# Coercion methods of interest:
# - from="RangedSummarizedExperiment", to="SummarizedExperiment"
# - from="SingleCellExperiment", to="RangedSummarizedExperiment"
# - from="SingleCellExperiment", to="SummarizedExperiment"



# Updated 2019-07-19.
`.coerce.RangedSummarizedExperiment,SummarizedExperiment` <-  # nolint
    function(from) {
        # Keep track of row metadata.
        rowMeta <- metadata(rowRanges(from))

        # Default coercion method in SummarizedExperiment package.
        method <- getMethod(
            f = "coerce",
            signature = signature(
                from = "RangedSummarizedExperiment",
                to = "SummarizedExperiment"
            ),
            where = asNamespace("SummarizedExperiment")
        )
        to <- method(from)

        # Reslot the row metadata.
        metadata(rowData(to)) <- rowMeta

        to
    }



# nolint start

# Can't set this as S4 coercion method because RSE and SE aren't local,
# modifiable classes in the package.
#
# If you attempt to define `coerce` method with `setAs()`, this will error:
#
# > Error in setIs(from, to, test = test, coerce = def, replace = replace,  :
# >   cannot create a 'setIs' relation when neither of the classes
# >   (“RangedSummarizedExperiment” and “SummarizedExperiment”) is local and
# >   modifiable in this package

# #' @rdname coerce-SummarizedExperiment
# #' @name coerce,RangedSummarizedExperiment,SummarizedExperiment-method
# setAs(
#     from = "RangedSummarizedExperiment",
#     to = "SummarizedExperiment",
#     def = `.coerce.RangedSummarizedExperiment,SummarizedExperiment`
# )

# nolint end
