data(
    DataFrame,
    data.table,
    IRanges,
    GRanges,
    RangedSummarizedExperiment,
    sparseMatrix,
    tbl_df,
    package = "acidtest",
    envir = environment()
)

stopifnot(
    is(DataFrame, "DataFrame"),
    is(data.table, "data.table"),
    is(IRanges, "IRanges"),
    is(GRanges, "GRanges"),
    is(RangedSummarizedExperiment, "RangedSummarizedExperiment"),
    is(sparseMatrix, "sparseMatrix"),
    is(tbl_df, "tbl_df")
)

df <- DataFrame
dt <- data.table
ir <- IRanges
gr <- GRanges
rse <- RangedSummarizedExperiment
sparse <- sparseMatrix
tbl <- tbl_df

## nolint start
DataFrame <- S4Vectors::DataFrame
colData <- SummarizedExperiment::colData
data.table <- data.table::data.table
isSubset <- goalie::isSubset
rowData <- SummarizedExperiment::rowData
rowRanges <- SummarizedExperiment::rowRanges
tibble <- tibble::tibble
## nolint end
