context("encode")

test_that("DataFrame", {
    x <- encode(df)
    expect_s4_class(x[[1L]], "Rle")
})

test_that("GRanges", {
    x <- encode(gr)
    expect_s4_class(mcols(x)[[1L]], "Rle")
})

## FIXME Add support for SummarizedExperiment
