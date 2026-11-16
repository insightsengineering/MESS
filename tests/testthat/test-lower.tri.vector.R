context("Test lower.tri.vector")

test_that("Matches documented example with clusters", {
    m <- matrix(1:64, ncol = 8)
    cluster <- c(1, 1, 1, 1, 2, 2, 3, 3)
    expect_equal(lower.tri.vector(m, cluster), c(2, 3, 4, 11, 12, 20, 38, 56))
})

test_that("diag=TRUE includes the diagonal elements", {
    m <- matrix(1:64, ncol = 8)
    cluster <- c(1, 1, 1, 1, 2, 2, 3, 3)
    expect_equal(
        lower.tri.vector(m, cluster, diag = TRUE),
        c(1, 2, 3, 4, 10, 11, 12, 19, 20, 28, 37, 38, 46, 55, 56, 64)
    )
})

test_that("Default cluster treats the whole matrix as one block", {
    m <- matrix(1:64, ncol = 8)
    expect_equal(length(lower.tri.vector(m)), sum(lower.tri(m)))
})

test_that("Mismatched cluster length raises an error", {
    m <- matrix(1:64, ncol = 8)
    expect_error(lower.tri.vector(m, c(1, 1)), "Length of cluster vector must match")
})

test_that("Non-contiguous cluster blocks raise an error", {
    m <- matrix(1:64, ncol = 8)
    expect_error(
        lower.tri.vector(m, cluster = c(2, 1, 1, 1, 1, 1, 2, 2)),
        "contiguous blocks"
    )
})
