# Fixed data with known bin counts: (0,1] -> 1, (1,2] -> 2, (2,3] -> 3
root_x <- c(1, 2, 2, 3, 3, 3)

test_that("rootonorm invisibly returns the histogram counts", {
  withr::local_pdf(NULL)

  expect_invisible(res <- rootonorm(root_x, breaks = 0:3))
  expect_equal(res, c(1, 2, 3))
})

test_that("rootonorm counts match hist() for the default breaks", {
  withr::local_pdf(NULL)
  withr::local_seed(1)
  x <- rnorm(200)
  expect_equal(rootonorm(x), hist(x, plot = FALSE)$counts)
})

test_that("all plot types, scales and reference distributions can be drawn", {
  withr::local_pdf(NULL)

  expect_equal(rootonorm(root_x, breaks = 0:3, type = "deviation"), c(1, 2, 3))
  expect_equal(rootonorm(root_x, breaks = 0:3, scale = "raw"), c(1, 2, 3))
  expect_equal(rootonorm(root_x, breaks = 0:3, mu = 1, s = 2, zeroline = FALSE, ylim = c(-1, 3)), c(1, 2, 3))
})

test_that("invalid inputs raise informative errors", {
  expect_error(rootonorm("a"), "'x' must be numeric")
  expect_error(rootonorm(root_x, breaks = c(0, 1, 3)), "breaks must be equally spaced")
})
