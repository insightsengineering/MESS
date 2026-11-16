context("Test sinv")

test_that("Inverse of a symmetric positive-definite matrix multiplies to the identity", {
    m <- matrix(c(1, 0, .5, .5, 0, 1, .5, .5, .5, .5, 1, .5, .5, .5, .5, 1), 4)
    result <- sinv(m) %*% m
    expect_equal(unname(result), diag(4), tolerance = 1e-8)
})

test_that("Returns a matrix of the same dimensions as the input", {
    m <- diag(3) * 2
    result <- sinv(m)
    expect_equal(dim(result), dim(m))
    expect_equal(result, diag(3) * 0.5)
})

test_that("A non positive-definite matrix raises an error from chol()", {
    m <- matrix(c(1, 2, 2, 1), 2)
    expect_error(sinv(m))
})
