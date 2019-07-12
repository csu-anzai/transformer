context("coerce to SummarizedExperiment")

# This method improves rowData handling for classes that extend RSE.
test_that("Object extending RangedSummarizedExperiment", {
    x <- as.SummarizedExperiment(rse)
    expect_s4_class(x, "SummarizedExperiment")
    # Check that rowData doesn't get dropped.
    expect_identical(
        object = as.character(rowData(x)[["geneID"]])[[1L]],
        expected = "ENSG00000000003"
    )
})

# Easy way to test an S4 class that extends SE without importing?
test_that("Object extending SummarizedExperiment", {
    se <- as(rse, "SummarizedExperiment")
    # Code coverage for `rowMeta` handling.
    data <- as.SummarizedExperiment(se)
    expect_identical(names(rowData(data))[[1L]], "geneID")
})
