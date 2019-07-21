context("decode")

test_that("DataFrame", {
    x <- encode(df)
    y <- decode(x)
    expect_is(y[[1L]], "integer")
})

test_that("GRanges", {
    x <- encode(gr)
    y <- decode(x)
    expect_is(mcols(y)[[1L]], "character")
})

## FIXME Add coverage for SummarizedExperiment
