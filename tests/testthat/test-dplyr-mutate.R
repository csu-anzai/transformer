context("dplyr : mutate")

x <- mtcars

test_that("mutate_all", {
    x <- mutate_all(x, .funs = log, base = 2L)
    expect_s4_class(x, "DataFrame")
    expect_true(hasRownames(x))
})

test_that("mutate_at", {
    x <- mutate_at(x, .vars = c("mpg", "cyl"), log, base = 2L)
    expect_s4_class(x, "DataFrame")
    expect_true(hasRownames(x))
})

test_that("mutate_if", {
    x <- mutate_if(x, .predicate = is.double, .funs = as.integer)
    expect_s4_class(x, "DataFrame")
    expect_true(hasRownames(x))
})

test_that("transmute_at", {
    x <- transmute_at(x, .vars = c("mpg", "cyl"), log, base = 2L)
    expect_s4_class(x, "DataFrame")
    expect_true(hasRownames(x))
})

test_that("transmute_if", {
    x <- transmute_if(x, .predicate = is.double, .funs = as.integer)
    expect_s4_class(x, "DataFrame")
    expect_true(hasRownames(x))
})
