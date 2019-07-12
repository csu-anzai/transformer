context("Coerce to DataFrame")

with_parameters_test_that(
    "S4 `as()` coercion", {
        x <- as(object, "DataFrame")
        expect_s4_class(x, "DataFrame")
        expect_true(hasRownames(x))
    },
    object = list(dt, sparse, tbl)
)
