context("Test ordered.clusters")

test_that("Detects non-contiguous clusters", {
    x <- c(1, 1, 1, 2, 2, 3, 4, 1, 5, 5, 5)
    expect_false(ordered.clusters(x))
})

test_that("Detects contiguous clusters after sorting", {
    x <- c(1, 1, 1, 2, 2, 3, 4, 1, 5, 5, 5)
    expect_true(ordered.clusters(sort(x)))
    expect_true(ordered.clusters(x[order(x)]))
})

test_that("A vector with no duplicates is trivially contiguous", {
    expect_true(ordered.clusters(c(1, 2, 3)))
    expect_true(ordered.clusters(1))
})
