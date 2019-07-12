data(
    df, dt, ir, gr, rse, sparse, tbl,
    package = "acidtest",
    envir = environment()
)

stopifnot(
    is(df, "DataFrame"),
    is(dt, "data.table"),
    is(ir, "IRanges"),
    is(gr, "GRanges"),
    is(rse, "RangedSummarizedExperiment"),
    is(sparse, "sparseMatrix"),
    is(tbl, "tbl_df")
)

DataFrame <- S4Vectors::DataFrame  # nolint
colData <- SummarizedExperiment::colData
data.table <- data.table::data.table  # nolint
isSubset <- goalie::isSubset
rowData <- SummarizedExperiment::rowData
rowRanges <- SummarizedExperiment::rowRanges
tibble <- tibble::tibble
