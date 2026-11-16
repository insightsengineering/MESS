context("Test rnorm_perfect")

test_that("Is deterministic: repeated calls with the same arguments are identical", {
    v1 <- rnorm_perfect(30, mean = 10, sd = 2)
    v2 <- rnorm_perfect(30, mean = 10, sd = 2)
    expect_identical(v1, v2)
    expect_length(v1, 30)
})

test_that("With mean=0 the sample mean of the result is essentially zero", {
    v <- rnorm_perfect(30, mean = 0, sd = 2)
    expect_equal(mean(v), 0, tolerance = 1e-10)
})

test_that("Scales symmetrically for odd n, including an exact zero at the midpoint", {
    v <- rnorm_perfect(5, mean = 0, sd = 1)
    expect_equal(v[3], 0)
    expect_equal(v, -rev(v), tolerance = 1e-8)
})

test_that("Non-positive or invalid n raises an error", {
    expect_error(rnorm_perfect(-5))
    expect_error(rnorm_perfect(0))
    expect_error(rnorm_perfect(5, mean = c(0, 1)))
    expect_error(rnorm_perfect(5, sd = c(1, 2)))
})
