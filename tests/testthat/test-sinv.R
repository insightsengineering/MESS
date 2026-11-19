test_that("sinv inverts a symmetric positive-definite matrix", {
  m <- matrix(c(1, 0, .5, .5,
                0, 1, .5, .5,
                .5, .5, 1, .5,
                .5, .5, .5, 1), 4)
  expect_equal(sinv(m), solve(m))
  expect_equal(sinv(m) %*% m, diag(4))
})

test_that("sinv of a diagonal matrix inverts the diagonal", {
  expect_equal(sinv(diag(c(2, 4, 5))), diag(c(0.5, 0.25, 0.2)))
})

test_that("a matrix that is not positive definite raises an error", {
  expect_error(sinv(matrix(c(1, 2, 2, 1), 2)), "not positive")
})
