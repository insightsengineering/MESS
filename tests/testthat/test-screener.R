context("Test screen_variables")

test_that("default global-strong method returns the documented list structure", {
    set.seed(10)
    x <- matrix(rnorm(30*10), nrow=30)
    y <- rnorm(30, mean=x[,1])

    lambda <- c(.1, 1, 5)
    res <- screen_variables(x, y, lambda=lambda)

    expect_named(res, c("lambda", "selected", "method"))
    expect_equal(res$lambda, lambda)
    expect_equal(res$method, "global-strong")
})

test_that("global-strong selection count changes monotonically with lambda (direction as observed)", {
    set.seed(10)
    x <- matrix(rnorm(30*10), nrow=30)
    y <- rnorm(30, mean=x[,1])

    # Choose lambda values straddling lambdamax/2 so that the screening
    # criterion actually discriminates between variables (otherwise all
    # lambda values give an empty selection for every column).
    res_check <- abs(as.vector(crossprod(x, y)))
    lambdamax <- max(res_check)
    lambda <- lambdamax * c(0.4, 0.6, 0.9)

    res <- screen_variables(x, y, lambda=lambda)

    expect_type(res$selected, "list")
    expect_length(res$selected, length(lambda))

    counts <- sapply(res$selected, length)
    # Empirically, for the "global-strong" criterion (2*lambda - lambdamax),
    # increasing lambda increases the criterion and therefore increases the
    # number of indices satisfying res < criterion, i.e. the count is
    # non-decreasing in lambda.
    expect_true(all(diff(counts) >= 0))
})

test_that("accepts a table as input for x", {
    set.seed(5)
    tbl <- table(sample(1:3, 30, replace=TRUE), sample(1:2, 30, replace=TRUE))
    y <- rnorm(nrow(tbl))

    res <- screen_variables(tbl, y, lambda=c(.1, 1))

    expect_equal(res$lambda, c(.1, 1))
    expect_equal(res$method, "global-strong")
})

test_that("global-DPP method currently errors due to norm() requiring a character type", {
    set.seed(10)
    x <- matrix(rnorm(30*10), nrow=30)
    y <- rnorm(30, mean=x[,1])

    # The source computes norm(x, 2) with a numeric second argument, but
    # base::norm() requires `type` to be a character string, so this method
    # always fails for matrix input under the current R behaviour.
    expect_error(screen_variables(x, y, lambda=c(.1, 1), method="global-DPP"))
})

test_that("non-matrix/non-table x raises an error", {
    set.seed(10)
    x <- matrix(rnorm(30*10), nrow=30)
    y <- rnorm(30, mean=x[,1])

    expect_error(screen_variables(as.data.frame(x), y), "matrix or table")
    expect_error(screen_variables(as.vector(x), y), "matrix or table")
})

test_that("non-positive lambda raises an error", {
    set.seed(10)
    x <- matrix(rnorm(30*10), nrow=30)
    y <- rnorm(30, mean=x[,1])

    expect_error(screen_variables(x, y, lambda=0), "positive")
    expect_error(screen_variables(x, y, lambda=-1), "positive")
    expect_error(screen_variables(x, y, lambda=c(.1, -1)), "positive")
})
