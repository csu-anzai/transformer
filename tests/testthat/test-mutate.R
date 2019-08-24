context("mutate")

x <- mtcars

test_that("mutateAll", {
    x <- mutateAll(x, .fun = log, base = 2L)
    expect_s4_class(x, "DataFrame")
    expect_true(hasRownames(x))
})

test_that("mutateAt", {
    x <- mutateAt(x, .vars = c("mpg", "cyl"), log, base = 2L)
    expect_s4_class(x, "DataFrame")
    expect_true(hasRownames(x))
})

test_that("mutateIf", {
    x <- mutateIf(x, .predicate = is.double, .fun = as.integer)
    expect_s4_class(x, "DataFrame")
    expect_true(hasRownames(x))
})

test_that("transmuteAt", {
    x <- transmuteAt(x, .vars = c("mpg", "cyl"), log, base = 2L)
    expect_s4_class(x, "DataFrame")
    expect_true(hasRownames(x))
})

test_that("transmuteIf", {
    x <- transmuteIf(x, .predicate = is.double, .fun = as.integer)
    expect_s4_class(x, "DataFrame")
    expect_true(hasRownames(x))
})
