context("Coercion methods")

load(system.file("extdata", "rse.rda", package = "transformer"))
load(system.file("extdata", "sparseMatrix.rda", package = "transformer"))
load(system.file("extdata", "tbl_df.rda", package = "transformer"))

DataFrame <- S4Vectors::DataFrame  # nolint
colData <- SummarizedExperiment::colData
data.table <- data.table::data.table  # nolint
rowData <- SummarizedExperiment::rowData
rowRanges <- SummarizedExperiment::rowRanges
tibble <- tibble::tibble


# data.frame ===================================================================
# Note that `as(object, "data.frame")` will keep tbl_df class here.
with_parameters_test_that(
    "as data.frame", {
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



# data.table ===================================================================
test_that("data.frame to data.table", {
    data <- data.frame()
    data <- as(data, "data.table")
    expect_is(data, "data.table")
})

test_that("DataFrame to data.table", {
    data <- colData(rse)
    x <- as(data, "data.table")
    expect_is(x, "data.table")

    # Expect that rownames are automatically moved to first column.
    expect_identical(colnames(x)[[1L]], "rowname")

    # Early return if already data.table.
    x <- data.table()
    expect_identical(x, as(x, "data.table"))

    # Coercion of a DataFrame containing a list column is allowed.
    data <- DataFrame()
    data[["x"]] <- list()
    data <- as(data, "data.table")
    expect_is(data, "data.table")

    # Error on complex S4 column (e.g. GRanges).
    data <- as(rowRanges(rse), "DataFrame")
    expect_s4_class(data[["X"]], "GRanges")
    # Expect error on "X" column, which contains nested GRanges.
    expect_error(
        object = as(data, "data.table"),
        regexp = "X"
    )

    # Check handling when rownames are NULL.
    data <- DataFrame(a = 1L, b = "b")
    expect_null(rownames(data))
    data <- as(data, "data.table")
    expect_is(data, "data.table")
    # Note that tibble doesn't support row names, but they still return like
    # standard data.frame class, where you can't actually set NULL.
    expect_identical(rownames(data), "1")
})

test_that("GRanges to data.table", {
    data <- rowRanges(rse)
    data <- as(data, "data.table")
    expect_is(data, "data.table")
})



# DataFrame ====================================================================
with_parameters_test_that(
    "as DataFrame", {
        x <- as(object, "DataFrame")
        expect_s4_class(x, "DataFrame")
    },
    object = list(
        sparseMatrix,
        tbl_df
    )
)

test_that("tbl_df to DataFrame", {
    # Check for rownames column and move automatically.
    data <- tibble(rowname = "test", a = 1L)
    data <- as(data, "DataFrame")
    expect_s4_class(data, "DataFrame")
    expect_identical(
        object = data,
        expected = DataFrame(a = 1L, row.names = "test")
    )
})



# SummarizedExperiment =========================================================
# This method improves rowData handling for classes that extend RSE.
test_that("Extends RangedSummarizedExperiment", {
    expect_identical(
        object = as(rse, "SummarizedExperiment"),
        expected = as.SummarizedExperiment(rse)
    )
    x <- as.SummarizedExperiment(rse)
    # Check that rowData doesn't get dropped.
    expect_identical(
        object = rowData(x)[["featureID"]][[1L]],
        expected = "ID001"
    )
})

# Easy way to text an S4 class that extends SE without importing?
test_that("Extends SummarizedExperiment", {
    se <- as(rse, "SummarizedExperiment")
    # Code coverage for `rowMeta` handling.
    data <- as.SummarizedExperiment(se)
    expect_identical(names(rowData(data)), "featureID")
})



# tbl_df =======================================================================
test_that("data.frame to tbl_df", {
    data <- data.frame()
    data <- as(data, "tbl_df")
    expect_is(data, "tbl_df")
})

test_that("DataFrame to tbl_df", {
    data <- colData(rse)
    x <- as(data, "tbl_df")
    expect_is(x, "tbl_df")

    # Expect that rownames are automatically moved to first column.
    expect_identical(colnames(x)[[1L]], "rowname")

    # Early return if already tibble.
    x <- tibble()
    expect_identical(x, as(x, "tbl_df"))

    # Coercion of a DataFrame containing a list column is allowed.
    data <- DataFrame()
    data[["x"]] <- list()
    data <- as(data, "tbl_df")
    expect_is(data, "tbl_df")

    # Error on complex S4 column (e.g. GRanges).
    data <- as(rowRanges(rse), "DataFrame")
    expect_s4_class(data[["X"]], "GRanges")
    # Expect error on "X" column, which contains nested GRanges.
    expect_error(
        object = as(data, "tbl_df"),
        regexp = "X"
    )

    # Check handling when rownames are NULL.
    data <- DataFrame(a = 1L, b = "b")
    expect_null(rownames(data))
    data <- as(data, "tbl_df")
    expect_is(data, "tbl_df")
    # Note that tibble doesn't support row names, but they still return like
    # standard data.frame class, where you can't actually set NULL.
    expect_identical(rownames(data), "1")
})

test_that("GRanges to tbl_df", {
    data <- rowRanges(rse)
    data <- as(data, "tbl_df")
    expect_is(data, "tbl_df")
})
