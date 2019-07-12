context("Coerce to DataFrame")

with_parameters_test_that(
    "S4 `as()` coercion", {
        x <- as(object, "DataFrame")
        expect_s4_class(x, "DataFrame")
    },
    object = list(sparse, tbl)
)

test_that("tbl_df row names", {
    # Check for rownames column and move automatically.
    data <- tibble(rowname = "test", a = 1L)
    data <- as(data, "DataFrame")
    expect_s4_class(data, "DataFrame")
    expect_identical(
        object = data,
        expected = DataFrame(a = 1L, row.names = "test")
    )
})
