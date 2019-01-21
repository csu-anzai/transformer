context("Coercion methods")

load(system.file("extdata", "rse.rda", package = "transformer"))

load(system.file("extdata", "sparseMatrix.rda", package = "transformer"))
load(system.file("extdata", "tbl_df.rda", package = "transformer"))

rowData <- SummarizedExperiment::rowData



# data.frame ===================================================================
# Note that `as(object, "data.frame")` will keep tbl_df class here.
with_parameters_test_that(
    "as.data.frame", {
        x <- as(object, "data.frame")
        expect_is(x, "data.frame")

        x <- as.data.frame(object)
        expect_is(x, "data.frame")
    },
    object = list(
        sparseMatrix,
        tbl_df
    )
)



# DataFrame ====================================================================
with_parameters_test_that(
    "as.DataFrame", {
        x <- as(object, "DataFrame")
        expect_s4_class(x, "DataFrame")
    },
    object = list(
        sparseMatrix,
        tbl_df
    )
)



# SummarizedExperiment =========================================================
test_that("as SummarizedExperiment", {
    expect_identical(
        object = as(rse, "SummarizedExperiment"),
        expected = as.SummarizedExperiment(rse)
    )
    x <- as(rse, "SummarizedExperiment")
    # Check that rowData doesn't get dropped.
    expect_identical(
        object = rowData(x)[["featureID"]][[1L]],
        expected = "ID001"
    )
})



# tbl_df =======================================================================
#' df <- SummarizedExperiment::colData(rse)
#' gr <- SummarizedExperiment::rowRanges(rse)
#'
#' ## DataFrame to tbl_df ====
#' x <- as(df, "tbl_df")
#' x <- as_tibble(df)
#' print(x)
#'
#' ## GRanges to tbl_df ====
#' x <- as(gr, "tbl_df")
#' x <- as_tibble(gr)
#' print(x)


test_that("DataFrame to tbl_df", {
    data <- SummarizedExperiment::colData(rse)
    x <- as(data, "tbl_df")
    expect_is(x, "tbl_df")

    # Expect that rownames are automatically moved to first column.
    expect_identical(colnames(x)[[1L]], "rowname")

    # Early return if already tibble.
    x <- tibble::tibble()
    expect_identical(x, as(x, "tbl_df"))
})
