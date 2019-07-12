context("Join operations")

test_that("left_join : Matched rows", {
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

test_that("left_join : Unmatched rows", {
    df1 <- DataFrame(
        id = as.factor(seq(4L)),
        genotype = as.factor(rep(x = c("wt", "ko"), each = 2L))
    )
    df2 <- DataFrame(
        id = as.factor(seq(4L)),
        treatment = as.factor(rep(x = c("control", "expt"), times = 2L))
    )
    # Reverse the row order of df2.
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

test_that("left_join: Uneven rows", {
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
