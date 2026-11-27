# Matrix helpers implemented in C++ (src/), cross-checked against base R.

test_that("add_torows() adds a vector to every row", {
  A <- matrix(1:12, ncol = 3)
  expect_equal(add_torows(A, c(1, 2, 3)), t(t(A) + c(1, 2, 3)))
  expect_equal(add_torows(matrix(5), 2), matrix(7))
})

test_that("add_torows() errors when the vector length differs from ncol", {
  expect_error(add_torows(matrix(1:12, ncol = 3), c(1, 2)), "Length of v does not match columns")
})

test_that("colCumSum() equals apply(m, 2, cumsum)", {
  expect_equal(colCumSum(matrix(1:6, 2)), matrix(c(1, 3, 3, 7, 5, 11), 2))
  withr::local_seed(1)
  m <- matrix(rnorm(20), nrow = 5)
  expect_equal(colCumSum(m), apply(m, 2, cumsum))
  expect_equal(colCumSum(matrix(1:3, nrow = 1)), matrix(1:3, nrow = 1))
})

test_that("cmd() equals 1 - tr(AB) / (||A||_F ||B||_F)", {
  m1 <- matrix(1, 4, 4)
  m2 <- matrix(c(1, 0, .5, .5, 0, 1, .5, .5, .5, .5, 1, .5, .5, .5, .5, 1), 4)
  m3 <- matrix(c(1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1), 4)
  cmd_r <- function(a, b) 1 - sum(diag(a %*% b)) / (norm(a, "F") * norm(b, "F"))

  expect_equal(cmd(m1, m1), 0)
  expect_equal(cmd(m1, m2), cmd_r(m1, m2))
  expect_equal(cmd(m2, m3), cmd_r(m2, m3))
  # documented example values
  expect_equal(cmd(m1, m2), 0.1174774, tolerance = 1e-6)
  expect_equal(cmd(m2, m3), 0.3594874, tolerance = 1e-6)
})

test_that("cmd() errors for matrices of different dimensions", {
  expect_error(cmd(matrix(1:4, 2), matrix(1:9, 3)), "must have the same dimensions")
})

test_that("qdiag() equals diag()", {
  withr::local_seed(2)
  m <- matrix(rnorm(16), nrow = 4)
  expect_equal(qdiag(m), diag(m))
  expect_equal(qdiag(matrix(5)), 5)
})

test_that("qdiag() errors for non-square or non-matrix input", {
  expect_error(qdiag(matrix(1:6, nrow = 2)), "must be square")
  expect_error(qdiag(matrix(1:6, nrow = 3)), "must be square")
  expect_error(qdiag(1:3), "Not a matrix")
})

test_that("quadform() computes t(x) M x and its variants", {
  x <- matrix(1:4, ncol = 2)
  M <- matrix(c(4, 1, 1, 3), ncol = 2)

  expect_equal(quadform(x, M), t(x) %*% M %*% x)
  expect_equal(quadform(x, M, invertM = TRUE), t(x) %*% solve(M) %*% x)
  expect_equal(quadform(t(x), M, transposex = TRUE), t(x) %*% M %*% x)
  expect_equal(quadform(t(x), M, invertM = TRUE, transposex = TRUE), t(x) %*% solve(M) %*% x)
  expect_equal(quadform(matrix(2), matrix(3)), matrix(12))
})

test_that("quadform() errors for non-conformable input", {
  # the message comes from Armadillo, so only the error is checked
  expect_error(quadform(matrix(1:6, nrow = 2), matrix(1:9, nrow = 3)))
})

test_that("repmat() tiles a matrix", {
  m <- matrix(1:6, ncol = 3)
  expect_equal(repmat(m, 2), rbind(m, m))
  expect_equal(repmat(m, 2, 3), cbind(rbind(m, m), rbind(m, m), rbind(m, m)))
})

test_that("repmat() errors for fewer than one replicate", {
  m <- matrix(1:6, ncol = 3)
  expect_error(repmat(m, 0, 1), "Number of rows to replicate must be >= 1")
  expect_error(repmat(m, 1, 0), "Number of columns to replicate must be >= 1")
})

test_that("tracemp() equals tr(t(A) B)", {
  A <- matrix(1:12, ncol = 3)
  expect_equal(tracemp(A, A), 650) # sum(A^2) = sum((1:12)^2)
  withr::local_seed(3)
  A <- matrix(rnorm(12), ncol = 3)
  B <- matrix(rnorm(12), ncol = 3)
  expect_equal(tracemp(A, B), sum(diag(t(A) %*% B)))
})

test_that("tracemp() errors for matrices of different dimensions", {
  expect_error(tracemp(matrix(1:4, 2), matrix(1:9, 3)), "must have the same dimensions")
})
