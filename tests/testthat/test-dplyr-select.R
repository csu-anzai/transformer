context("dplyr : select")

x <- mtcars

test_that("select_all", {
    x <- select_all(x, .funs = toupper)
    expect_s4_class(x, "DataFrame")
    expect_true(hasRownames(x))
})

test_that("select_at", {
    x <- select_at(x, .vars = c("mpg", "cyl"))
    expect_s4_class(x, "DataFrame")
    expect_true(hasRownames(x))
})

test_that("select_if", {
    x <- select_if(x, .predicate = is.double)
    expect_s4_class(x, "DataFrame")
    expect_true(hasRownames(x))
})
