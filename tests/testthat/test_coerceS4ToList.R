context("Coerce S4 to list")

data(rse, package = "acidtest", envir = environment())

with_parameters_test_that(
    "Coerce S4 to list", {
        object <- fun(rse)
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
    },
    fun = list(coerceS4ToList, flatFiles)
)
