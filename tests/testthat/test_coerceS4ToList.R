context("Coerce S4 to list")

load(system.file("extdata", "rse.rda", package = "transformer"))



test_that("coerceS4ToList", {
    object <- coerceS4ToList(rse)
    expect_is(object, "list")
    expect_identical(
        object = names(object),
        expected = c(
            "rowRanges",
            "colData",
            "assays",
            "NAMES",
            "elementMetadata",
            "metadata"
        )
    )
})
