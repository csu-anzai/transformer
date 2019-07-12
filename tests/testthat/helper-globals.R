data(
    ir, gr, rse, sparse, tbl,
    package = "acidtest",
    envir = environment()
)

DataFrame <- S4Vectors::DataFrame  # nolint
colData <- SummarizedExperiment::colData
data.table <- data.table::data.table  # nolint
isSubset <- goalie::isSubset
rowData <- SummarizedExperiment::rowData
rowRanges <- SummarizedExperiment::rowRanges
tibble <- tibble::tibble
