context("select")

x <- mtcars

test_that("selectIf", {
    x <- selectIf(x, predicate = is.double)
    expect_s4_class(x, "DataFrame")
    expect_true(hasRownames(x))
})
