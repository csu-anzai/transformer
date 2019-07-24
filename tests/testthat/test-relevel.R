context("relevel")

Rle <- structure("Rle", package = "S4Vectors")  # nolint

test_that("DataFrame", {
    cd <- colData(rse)
    expect_s4_class(cd, "DataFrame")
    x <- relevel(cd)
    expect_s4_class(x, "DataFrame")
    expect_identical(
        object = lapply(x, class),
        expected = list(condition = "factor")
    )
})

test_that("Early return on empty DataFrame", {
    expect_identical(relevel(DataFrame()), DataFrame())
})

test_that("IRanges", {
    expect_identical(relevel(ir), ir)
})

test_that("GRanges", {
    rr <- rowRanges(rse)
    expect_s4_class(rr, "GRanges")
    x <- relevel(rr)
    expect_s4_class(x, "GRanges")
    if (packageVersion("GenomicRanges") < "1.31") {
        AsIs <- "AsIs"  # nolint
    } else {
        AsIs <- "list"  # nolint
    }
    expect_identical(
        object = lapply(mcols(x), class),
        expected = list(
            geneID = Rle,
            geneName = Rle,
            geneBiotype = Rle,
            broadClass = Rle,
            entrezID = AsIs
        )
    )
})

test_that("SummarizedExperiment", {
    x <- relevel(rse)
    expect_s4_class(x, "RangedSummarizedExperiment")
    ## Check for factor columns.
    ok <- any(vapply(
        X = decode(rowData(x)),
        FUN = is.factor,
        FUN.VALUE = logical(1L)
    ))
    expect_true(ok)
    ok <- any(vapply(
        X = decode(colData(x)),
        FUN = is.factor,
        FUN.VALUE = logical(1L)
    ))
    expect_true(ok)
})
