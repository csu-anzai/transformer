context("(As) coercion methods")

load(system.file("extdata", "rse.rda", package = "S4Transformer"))



# tibble =======================================================================
test_that("tibble", {
    data <- SummarizedExperiment::colData(rse)
    x <- as(data, "tbl_df")
    expect_is(x, "tbl_df")

    # Expect that rownames are automatically moved to first column.
    expect_identical(colnames(x)[[1L]], "rowname")

    # Early return if already tibble.
    x <- tibble::tibble()
    expect_identical(x, as(x, "tbl_df"))
})
