context("Test round_percent")

test_that("Rounded percentages sum to 100 (documented example)", {
    f <- c(1, 2, 1, 3, 2, 1, 2, 3, 1)
    set.seed(1)
    result <- round_percent(f)
    expect_equal(length(result), length(f))
    expect_equal(sum(result), 100)
})

test_that("ties='last' gives a deterministic, reproducible result", {
    f <- c(1, 2, 1, 3, 2, 1, 2, 3, 1)
    expect_equal(round_percent(f, ties = "last"), c(6, 12, 6, 19, 13, 6, 13, 19, 6))
    expect_equal(sum(round_percent(f, ties = "last")), 100)
})

test_that("decimals argument controls precision and preserves proportions", {
    expect_equal(round_percent(c(10, 20, 30, 40), decimals = 1), c(10, 20, 30, 40))
})

test_that("Negative values raise an error", {
    expect_error(round_percent(c(-1, 2)), "only works on non-negative vectors")
})

test_that("Non-numeric input raises an error", {
    expect_error(round_percent(c("a", "b")), "only works on numeric vectors")
})

test_that("Negative decimals raise an error", {
    expect_error(round_percent(c(1, 2), decimals = -1), "non-negative integer")
})
