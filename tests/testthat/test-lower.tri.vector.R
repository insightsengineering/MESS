test_that("lower.tri.vector without clusters equals m[lower.tri(m)]", {
  m <- matrix(1:16, ncol = 4)
  expect_identical(lower.tri.vector(m), m[lower.tri(m)])
  expect_identical(lower.tri.vector(m, diag = TRUE), m[lower.tri(m, diag = TRUE)])
})

test_that("lower.tri.vector extracts within-cluster blocks (documented example)", {
  m <- matrix(1:64, ncol = 8)
  cluster <- c(1, 1, 1, 1, 2, 2, 3, 3)
  # Block 1 = rows/cols 1:4, block 2 = 5:6, block 3 = 7:8
  expect_equal(lower.tri.vector(m, cluster), c(2, 3, 4, 11, 12, 20, 38, 56))
  expect_equal(
    lower.tri.vector(m, cluster, diag = TRUE),
    c(1, 2, 3, 4, 10, 11, 12, 19, 20, 28, 37, 38, 46, 55, 56, 64)
  )
})

test_that("lower.tri.vector works on sparse Matrix objects", {
  m <- matrix(1:16, ncol = 4)
  expect_equal(lower.tri.vector(Matrix::Matrix(m)), m[lower.tri(m)])
})

test_that("lower.tri.vector validates the cluster argument", {
  m <- matrix(1:64, ncol = 8)
  expect_error(lower.tri.vector(m, c(1, 1)), "Length of cluster vector must match")
  expect_error(
    lower.tri.vector(m, cluster = c(2, 1, 1, 1, 1, 1, 2, 2)),
    "contiguous blocks"
  )
})
