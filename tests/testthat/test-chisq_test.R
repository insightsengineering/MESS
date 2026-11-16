context("Test monte_carlo_chisq_test")

test_that("documented example (margin='N', default) returns a valid htest object", {
    set.seed(1)
    m <- matrix(c(12, 4, 8, 6), 2)
    res <- monte_carlo_chisq_test(m, B = 1000)

    expect_s3_class(res, "htest")
    expect_true(all(c("statistic", "parameter", "p.value", "method", "data.name",
                       "observed", "expected") %in% names(res)))
    expect_true(res$p.value >= 0 && res$p.value <= 1)
    expect_equal(res$observed, m)
})

test_that("margin='rows' returns a valid htest object with p.value in [0,1]", {
    set.seed(2)
    m <- matrix(c(12, 4, 8, 6), 2)
    res <- monte_carlo_chisq_test(m, margin = "rows", B = 1000)

    expect_s3_class(res, "htest")
    expect_true(res$p.value >= 0 && res$p.value <= 1)
    expect_match(res$method, "row margins")
})

test_that("margin='both' returns a valid htest object with p.value in [0,1]", {
    set.seed(3)
    m <- matrix(c(12, 4, 8, 6), 2)
    res <- monte_carlo_chisq_test(m, margin = "both", B = 1000)

    expect_s3_class(res, "htest")
    expect_true(res$p.value >= 0 && res$p.value <= 1)
    expect_match(res$method, "row AND column margins")
})

test_that("negative entries are rejected", {
    m <- matrix(c(-1, 4, 8, 6), 2)
    expect_error(monte_carlo_chisq_test(m, B = 100))
})

test_that("a zero row marginal is rejected when margin requires positive row sums", {
    m0 <- matrix(c(0, 4, 0, 6), 2) # first row is (0, 0)
    expect_error(monte_carlo_chisq_test(m0, margin = "rows", B = 100))
    expect_error(monte_carlo_chisq_test(m0, margin = "both", B = 100))
})

test_that("non-2-way input is rejected", {
    expect_error(monte_carlo_chisq_test(matrix(c(1, 2, 3), 1), B = 100))
    expect_error(monte_carlo_chisq_test(c(1, 2, 3, 4), B = 100))
})
