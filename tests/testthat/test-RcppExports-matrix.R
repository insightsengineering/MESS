context("Test RcppExports matrix helper functions")


## -----------------------------------------------------------------------
## add_torows
## -----------------------------------------------------------------------

test_that("add_torows adds a vector to each row of a matrix", {
    A <- matrix(1:12, ncol = 3)
    B <- c(1, 2, 3)
    expect_equal(add_torows(A, B), t(t(A) + B))
})

test_that("add_torows handles a 1x1 matrix", {
    expect_equal(add_torows(matrix(5), 2), matrix(7))
})

test_that("add_torows errors when vector length does not match number of columns", {
    A <- matrix(1:12, ncol = 3)
    expect_error(add_torows(A, c(1, 2)))
})


## -----------------------------------------------------------------------
## colCumSum
## -----------------------------------------------------------------------

test_that("colCumSum matches apply(m, 2, cumsum) for a random matrix", {
    set.seed(1)
    m <- matrix(rnorm(20), nrow = 5)
    expect_equal(colCumSum(m), apply(m, 2, cumsum))
})

test_that("colCumSum works for a single-row matrix", {
    m <- matrix(1:3, nrow = 1)
    expect_equal(colCumSum(m), m)
    expect_equal(dim(colCumSum(m)), dim(m))
})


## -----------------------------------------------------------------------
## cmd
## -----------------------------------------------------------------------

test_that("cmd is zero for identical correlation matrices", {
    m1 <- matrix(rep(1, 16), 4)
    expect_equal(cmd(m1, m1), 0)
})

test_that("cmd matches documented example values", {
    m1 <- matrix(rep(1, 16), 4)
    m2 <- matrix(c(1, 0, .5, .5, 0, 1, .5, .5, .5, .5, 1, .5, .5, .5, .5, 1), 4)
    m3 <- matrix(c(1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1), 4)
    expect_equal(cmd(m1, m2), 0.1174774, tolerance = 1e-6)
    expect_equal(cmd(m2, m3), 0.3594874, tolerance = 1e-6)
})

test_that("cmd handles 1x1 matrices", {
    expect_equal(cmd(matrix(1), matrix(1)), 0)
})

test_that("cmd errors when matrices have different dimensions", {
    expect_error(cmd(matrix(1:4, 2), matrix(1:9, 3)))
})


## -----------------------------------------------------------------------
## qdiag
## -----------------------------------------------------------------------

test_that("qdiag matches base R diag() for a square matrix", {
    set.seed(2)
    m <- matrix(rnorm(16), nrow = 4)
    expect_equal(qdiag(m), diag(m))
})

test_that("qdiag works for a 1x1 matrix", {
    expect_equal(qdiag(matrix(5)), 5)
})

test_that("qdiag errors for a non-square matrix", {
    expect_error(qdiag(matrix(1:6, nrow = 2, ncol = 3)))
    expect_error(qdiag(matrix(1:6, nrow = 3, ncol = 2)))
})

test_that("qdiag errors when given a non-matrix input", {
    expect_error(qdiag(1:3))
})


## -----------------------------------------------------------------------
## quadform
## -----------------------------------------------------------------------

test_that("quadform computes t(x) %*% M %*% x by default", {
    x <- matrix(1:4, ncol = 2)
    M <- matrix(c(2, 0, 0, 2), ncol = 2)
    expect_equal(quadform(x, M), t(x) %*% M %*% x)
})

test_that("quadform with invertM=TRUE inverts M before computing", {
    x <- matrix(1:4, ncol = 2)
    M <- matrix(c(4, 1, 1, 3), ncol = 2)
    expect_equal(quadform(x, M, invertM = TRUE), t(x) %*% solve(M) %*% x)
})

test_that("quadform with transposex=TRUE transposes x before computing", {
    x <- matrix(1:6, nrow = 2, ncol = 3)
    M <- matrix(c(2, 0, 0, 2), ncol = 2)
    xt <- t(x)
    expect_equal(quadform(xt, M, transposex = TRUE), xt %*% M %*% t(xt))
    expect_equal(quadform(xt, M, transposex = TRUE), quadform(x, M))
})

test_that("quadform works for a 1x1 case", {
    expect_equal(quadform(matrix(2), matrix(3)), matrix(12))
})

test_that("quadform errors when M dimensions are incompatible with x", {
    x <- matrix(1:6, nrow = 2, ncol = 3)
    badM <- matrix(1:9, nrow = 3, ncol = 3)
    expect_error(quadform(x, badM))
})


## -----------------------------------------------------------------------
## repmat
## -----------------------------------------------------------------------

test_that("repmat stacks copies of a matrix row-wise (ncol default = 1)", {
    m <- matrix(1:6, ncol = 3)
    expect_equal(repmat(m, 2), rbind(m, m))
})

test_that("repmat replicates a matrix row- and column-wise", {
    m <- matrix(1:6, ncol = 3)
    expected <- cbind(rbind(m, m), rbind(m, m), rbind(m, m))
    expect_equal(repmat(m, 2, 3), expected)
    expect_equal(dim(repmat(m, 2, 3)), c(4L, 9L))
})

test_that("repmat errors when nrow is less than 1", {
    m <- matrix(1:6, ncol = 3)
    expect_error(repmat(m, 0, 1))
})


## -----------------------------------------------------------------------
## tracemp
## -----------------------------------------------------------------------

test_that("tracemp computes trace(t(A) %*% B)", {
    set.seed(3)
    A <- matrix(rnorm(12), ncol = 3)
    B <- matrix(rnorm(12), ncol = 3)
    expect_equal(tracemp(A, B), sum(diag(t(A) %*% B)))
})

test_that("tracemp matches documented example with A == B", {
    A <- matrix(1:12, ncol = 3)
    expect_equal(tracemp(A, A), sum(diag(t(A) %*% A)))
    expect_equal(tracemp(A, A), 650)
})

test_that("tracemp errors when matrices have different dimensions", {
    expect_error(tracemp(matrix(1:4, 2), matrix(1:9, 3)))
})
