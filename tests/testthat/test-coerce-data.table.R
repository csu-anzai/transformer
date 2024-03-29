context("Coerce to data.table")

test_that("data.frame", {
    expect_is(as(data.frame(), "data.table"), "data.table")
})

test_that("DataFrame", {
    x <- as(df, "data.table")
    expect_is(x, "data.table")

    ## Expect that rownames are automatically moved to first column.
    expect_identical(colnames(x)[[1L]], "rn")

    ## Early return if already data.table.
    x <- data.table()
    expect_identical(x, as(x, "data.table"))

    ## Coercion of a DataFrame containing a list column is allowed.
    data <- DataFrame()
    data[["x"]] <- list()
    data <- as(data, "data.table")
    expect_is(data, "data.table")

    ## Error on complex S4 column (e.g. GRanges).
    data <- as(rowRanges(rse), "DataFrame")
    expect_s4_class(data[["X"]], "GRanges")
    ## Expect error on "X" column, which contains nested GRanges.
    expect_error(
        object = as(data, "data.table"),
        regexp = "X"
    )

    ## Check handling when rownames are NULL.
    data <- DataFrame(a = 1L, b = "b")
    expect_null(rownames(data))
    data <- as(data, "data.table")
    expect_is(data, "data.table")
    ## Note that tibble doesn't support row names, but they still return like
    ## standard data.frame class, where you can't actually set NULL.
    expect_identical(rownames(data), "1")
})

with_parameters_test_that(
    "Ranges", {
        expect_is(as.data.table(object), "data.table")
        expect_is(as(object, "data.table"), "data.table")
        expect_true(isSubset(
            x = "rn",
            y = colnames(as.data.table(object, keep.rownames = TRUE))
        ))
        expect_true(isSubset(
            x = "rowname",
            y = colnames(as.data.table(object, keep.rownames = "rowname"))
        ))
        expect_false(isSubset(
            x = "rowname",
            y = colnames(as.data.table(object, keep.rownames = FALSE))
        ))
        ## Kill names and cover automatic rowname handling.
        expect_false(isSubset(
            x = "rn",
            y = colnames(as.data.table(unname(object)))
        ))
    },
    object = list(gr, ir)
)
