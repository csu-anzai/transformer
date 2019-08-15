context("joins : Beatles vs. Stones (from dplyr)")

## nolint start
## Compare with data.frame original:
## > data(band_members, band_instruments, package = "dplyr")
## nolint end

data(band_members, band_instruments, envir = environment())
rownames(band_members) <- band_members[["name"]]
rownames(band_instruments) <- band_instruments[["name"]]

test_that("inner_join", {
    object <- inner_join(x = band_members, y = band_instruments, by = "name")
    expected <- DataFrame(
        name = c("John", "Paul"),
        band = c("Beatles", "Beatles"),
        plays = c("guitar", "bass"),
        row.names = c("John", "Paul")
    )
})

test_that("left_join", {
    object <- left_join(x = band_members, y = band_instruments, by = "name")
    expected <- DataFrame(
        name = c("Mick", "John", "Paul"),
        band = c("Stones", "Beatles", "Beatles"),
        plays = c(NA, "guitar", "bass"),
        row.names = c("Mick", "John", "Paul")
    )
    expect_identical(object, expected)
})

test_that("right_join", {
    object <- right_join(x = band_members, y = band_instruments, by = "name")
    expected <- DataFrame(
        name = c("John", "Paul", "Keith"),
        plays = c("guitar", "bass", "guitar"),
        band = c("Beatles", "Beatles", NA),
        row.names = c("John", "Paul", "Keith")
    )
    expect_identical(object, expected)
})

test_that("full_join", {
    object <- full_join(x = band_members, y = band_instruments, by = "name")
    expected <- DataFrame(
        name = c("Mick", "John", "Paul", "Keith"),
        band = c("Stones", "Beatles", "Beatles", NA),
        plays = c(NA, "guitar", "bass", "guitar"),
        row.names = c("Mick", "John", "Paul", "Keith")
    )
    expect_identical(object, expected)
})

test_that("semi_join", {
    object <- semi_join(x = band_members, y = band_instruments, by = "name")
    expected <- DataFrame(
        name = c("John", "Paul"),
        band = c("Beatles", "Beatles"),
        row.names = c("John", "Paul")
    )
    expect_identical(object, expected)
})

test_that("anti_join", {
    object <- anti_join(x = band_members, y = band_instruments, by = "name")
    expected <- DataFrame(
        name = "Mick",
        band = "Stones",
        row.names = "Mick"
    )
    expect_identical(object, expected)
})



context("joins : left_join")

test_that("Matched rows", {
    df1 <- DataFrame(
        id = as.factor(seq(4L)),
        genotype = as.factor(rep(x = c("wt", "ko"), each = 2L))
    )
    df2 <- DataFrame(
        id = as.factor(seq(4L)),
        treatment = as.factor(rep(x = c("control", "expt"), times = 2L))
    )
    expect_identical(
        object = left_join(df1, df2, by = "id"),
        expected = DataFrame(
            id = as.factor(seq(4L)),
            genotype = as.factor(rep(x = c("wt", "ko"), each = 2L)),
            treatment = as.factor(rep(x = c("control", "expt"), times = 2L))
        )
    )
})

test_that("Unmatched rows", {
    df1 <- DataFrame(
        id = as.factor(seq(4L)),
        genotype = as.factor(rep(x = c("wt", "ko"), each = 2L))
    )
    df2 <- DataFrame(
        id = as.factor(seq(4L)),
        treatment = as.factor(rep(x = c("control", "expt"), times = 2L))
    )
    ## Reverse the row order of df2.
    df2 <- df2[rev(seq_len(nrow(df2))), ]
    expect_identical(
        object = left_join(df1, df2, by = "id"),
        expected = DataFrame(
            id = as.factor(seq(4L)),
            genotype = as.factor(rep(x = c("wt", "ko"), each = 2L)),
            treatment = as.factor(rep(x = c("control", "expt"), times = 2L))
        )
    )
})

test_that("Uneven rows", {
    df1 <- DataFrame(
        id = as.factor(seq(4L)),
        genotype = as.factor(rep(x = c("wt", "ko"), each = 2L))
    )
    df2 <- DataFrame(
        id = as.factor(seq(2L)),
        treatment = as.factor(rep(x = c("control", "expt"), times = 1L))
    )
    expect_identical(
        object = left_join(df1, df2, by = "id"),
        expected = DataFrame(
            id = as.factor(seq(4L)),
            genotype = as.factor(rep(x = c("wt", "ko"), each = 2L)),
            treatment = as.factor(c("control", "expt", NA, NA))
        )
    )
})
