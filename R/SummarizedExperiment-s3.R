as.SummarizedExperiment <-  # nolint
    function(x) {
        UseMethod("as.SummarizedExperiment")
    }



# Note that our method here keeps track of `rowData` when coercing an object
# that extends RangedSummarizedExperiment to SummarizedExperiment. This bug
# needs to be fixed in the SummarizedExperiment package.
as.SummarizedExperiment.default <-  # nolint
    function(x) {
        if (is(x, "RangedSummarizedExperiment")) {
            rowMeta <- metadata(rowRanges(x))
            x <- as(x, "RangedSummarizedExperiment")
        } else {
            rowMeta <- metadata(rowData(x))
        }
        x <- as(x, "SummarizedExperiment")
        metadata(rowData(x)) <- rowMeta
        x
    }
