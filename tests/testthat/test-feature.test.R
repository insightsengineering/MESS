context("Test feature.test")

## NOTE on test design:
## Passing a numeric `s` sets `sfixed <- TRUE` internally, which skips the
## (slow, randomized) cv.glmnet call entirely -- both for the original fit
## and for every one of the B randomizations -- since it also forces
## keeplambda <- TRUE. This keeps these tests fast and deterministic.

test_that("returns the documented 7-field list, with NA statistics, when no variable is selected", {
    set.seed(42)
    x <- matrix(rnorm(20*5), nrow=20)
    y <- rnorm(20, mean=2*x[,1])

    set.seed(1)
    res <- feature.test(x, y, B=5, s=10, control=list(trace=FALSE, maxcores=1))

    expect_named(res, c("p.full", "ols.selected", "p.maxols",
                         "lasso.selected", "p.maxlasso", "lambda.orig", "B"))
    expect_true(is.na(res$p.full))
    expect_true(is.na(res$p.maxols))
    expect_true(is.na(res$p.maxlasso))
    expect_equal(res$lambda.orig, 10)
    expect_equal(res$B, 5)
})

test_that("B and lambda.orig are passed through unchanged for a different B/s combination", {
    set.seed(42)
    x <- matrix(rnorm(20*5), nrow=20)
    y <- rnorm(20, mean=2*x[,1])

    set.seed(1)
    res <- feature.test(x, y, B=3, s=20, control=list(trace=FALSE, maxcores=1))

    expect_equal(res$lambda.orig, 20)
    expect_equal(res$B, 3)
    expect_true(is.na(res$p.full))
})

test_that("a lambda that selects at least one variable currently errors (o.ols.order is never assigned)", {
    set.seed(42)
    x <- matrix(rnorm(20*5), nrow=20)
    y <- rnorm(20, mean=2*x[,1])

    # With this data/seed, s=0.2 makes the lasso fit select variable 1
    # (o.nselected > 0). In that branch the source computes o.tstat etc. but
    # never assigns o.ols.order (the assignment is commented out), while the
    # final result list indexes o.select[o.ols.order]. This currently raises
    # an "object 'o.ols.order' not found" error for every documented
    # normal-usage call where the lasso selects a non-empty set of variables.
    set.seed(1)
    expect_error(
        feature.test(x, y, B=5, s=0.2, control=list(trace=FALSE, maxcores=1)),
        "o.ols.order"
    )
})
