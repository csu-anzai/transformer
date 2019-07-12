context("coerce to data.table")

test_that("data.frame", {
    data <- data.frame()
    data <- as(data, "data.table")
    expect_is(data, "data.table")
})

test_that("DataFrame", {
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

test_that("GRanges", {
    data <- rowRanges(rse)
    data <- as(data, "data.table")
    expect_is(data, "data.table")
})
