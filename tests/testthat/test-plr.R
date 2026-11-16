context("Test mfastLmCpp and plr")


## -----------------------------------------------------------------------
## mfastLmCpp
## -----------------------------------------------------------------------

test_that("mfastLmCpp with intercept matches marginal simple regressions via lm", {
    set.seed(1)
    y <- rnorm(10)
    x <- matrix(rnorm(30), ncol = 3)

    res <- mfastLmCpp(y, x)

    expect_s3_class(res, "data.frame")
    expect_equal(names(res), c("coefficients", "stderr", "tstat"))
    expect_equal(nrow(res), ncol(x))

    for (j in seq_len(ncol(x))) {
        s <- coef(summary(lm(y ~ x[, j])))
        expect_equal(res$coefficients[j], unname(s[2, 1]))
        expect_equal(res$stderr[j], unname(s[2, 2]))
        expect_equal(res$tstat[j], unname(s[2, 3]))
    }
})

test_that("mfastLmCpp without intercept matches marginal regressions through the origin", {
    set.seed(1)
    y <- rnorm(10)
    x <- matrix(rnorm(30), ncol = 3)

    res <- mfastLmCpp(y, x, addintercept = FALSE)

    for (j in seq_len(ncol(x))) {
        s <- coef(summary(lm(y ~ x[, j] - 1)))
        expect_equal(res$coefficients[j], unname(s[1, 1]))
        expect_equal(res$stderr[j], unname(s[1, 2]))
        expect_equal(res$tstat[j], unname(s[1, 3]))
    }
})

test_that("mfastLmCpp handles a single-column regressor matrix", {
    set.seed(5)
    y <- rnorm(20)
    x <- matrix(rnorm(20), ncol = 1)

    res <- mfastLmCpp(y, x)
    s <- coef(summary(lm(y ~ x[, 1])))

    expect_equal(nrow(res), 1)
    expect_equal(res$coefficients[1], unname(s[2, 1]))
    expect_equal(res$stderr[1], unname(s[2, 2]))
    expect_equal(res$tstat[1], unname(s[2, 3]))
})


## -----------------------------------------------------------------------
## plr
## -----------------------------------------------------------------------

test_that("plr.numeric wraps mfastLmCpp for a vector outcome", {
    set.seed(42)
    N <- 50
    Nx <- 3
    y <- rnorm(N)
    x <- matrix(rnorm(N * Nx), ncol = Nx)

    expect_equal(plr(y, x), mfastLmCpp(y = y, x = x, addintercept = TRUE))
    expect_equal(plr(y, x, addintercept = FALSE),
                 mfastLmCpp(y = y, x = x, addintercept = FALSE))
})

test_that("plr.matrix applies mfastLmCpp to each column of a matrix outcome", {
    set.seed(42)
    N <- 50
    Nx <- 3
    Ny <- 4
    Y <- matrix(rnorm(N * Ny), ncol = Ny)
    X <- matrix(rnorm(N * Nx), ncol = Nx)

    res <- plr(Y, X)

    expect_type(res, "list")
    expect_length(res, ncol(Y))

    for (k in seq_len(ncol(Y))) {
        expect_equal(res[[k]], plr(Y[, k], X))
    }
})
