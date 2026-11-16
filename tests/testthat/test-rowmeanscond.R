context("Test conditional_rowMeans")

test_that("Produces documented results with default minobs", {
    expect_equal(conditional_rowMeans(1:5, c(1:4, NA), c(1:3, NA, NA)), c(1, 2, 3, 4, 5))
})

test_that("minobs=0 allows rows with all but one missing", {
    expect_equal(conditional_rowMeans(1:5, c(1:4, NA), c(1:3, NA, NA), minobs = 0), c(1, 2, 3, 4, 5))
})

test_that("minobs=2 returns NA when fewer than 2 non-missing observations", {
    expect_equal(conditional_rowMeans(1:5, c(1:4, NA), c(1:3, NA, NA), minobs = 2), c(1, 2, 3, 4, NA))
})

test_that("Non-conformable vector lengths trigger a recycling warning", {
    expect_warning(conditional_rowMeans(1:3, 1:4), "not a multiple")
})
