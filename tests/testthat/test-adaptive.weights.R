context("Test adaptive.weights")

test_that("multivariate method matches manually-computed lm coefficients", {
    set.seed(1)
    x <- matrix(rnorm(50*5), nrow=50)
    y <- rnorm(50, mean=x[,1])

    res <- adaptive.weights(x, y)
    manual <- 1/abs(coef(lm(y ~ x))[-1])^1

    expect_equal(unname(res$weights), unname(manual))
    expect_equal(res$nu, 1)
    expect_length(res$weights, ncol(x))
})

test_that("univariate method matches manually-computed per-column lm coefficients", {
    set.seed(1)
    x <- matrix(rnorm(50*5), nrow=50)
    y <- rnorm(50, mean=x[,1])

    res <- adaptive.weights(x, y, weight.method="univariate")
    manual <- apply(x, 2, function(xi) 1/abs(coef(lm(y ~ xi))[2]))

    expect_equal(unname(res$weights), unname(manual))
})

test_that("nu=0 gives all weights equal to 1 regardless of method", {
    set.seed(1)
    x <- matrix(rnorm(50*5), nrow=50)
    y <- rnorm(50, mean=x[,1])

    res_multi <- adaptive.weights(x, y, nu=0)
    res_uni   <- adaptive.weights(x, y, nu=0, weight.method="univariate")

    expect_equal(unname(res_multi$weights), rep(1, ncol(x)))
    expect_equal(unname(res_uni$weights), rep(1, ncol(x)))
})

test_that("weights scale predictably with nu (nu=2 is the square of nu=1)", {
    set.seed(1)
    x <- matrix(rnorm(50*5), nrow=50)
    y <- rnorm(50, mean=x[,1])

    res1 <- adaptive.weights(x, y, nu=1)
    res2 <- adaptive.weights(x, y, nu=2)

    expect_equal(unname(res2$weights), unname(res1$weights)^2)
})

test_that("falls back to univariate weights with a warning when p > n", {
    set.seed(2)
    x <- matrix(rnorm(10*15), nrow=10)
    y <- rnorm(10, mean=x[,1])

    expect_warning(res <- adaptive.weights(x, y), regexp="univariate")

    manual <- apply(x, 2, function(xi) 1/abs(coef(lm(y ~ xi))[2]))
    expect_equal(unname(res$weights), unname(manual))
    expect_length(res$weights, ncol(x))
})

test_that("invalid weight.method raises an error", {
    set.seed(1)
    x <- matrix(rnorm(50*5), nrow=50)
    y <- rnorm(50, mean=x[,1])

    expect_error(adaptive.weights(x, y, weight.method="bogus"))
})
