test_that("rootonorm returns counts matching hist() for default usage", {
  pdf(tempfile())
  on.exit(dev.off())
  set.seed(1)
  x <- rnorm(200)
  result <- rootonorm(x)
  expect_true(is.numeric(result))
  expect_equal(length(result), length(hist(x, plot = FALSE)$counts))
  expect_equal(as.numeric(result), as.numeric(hist(x, plot = FALSE)$counts))
})

test_that("rootonorm works with type='deviation'", {
  pdf(tempfile())
  on.exit(dev.off())
  set.seed(2)
  x <- rnorm(200)
  expect_no_error(rootonorm(x, type = "deviation"))
})

test_that("rootonorm works with type='hanging'", {
  pdf(tempfile())
  on.exit(dev.off())
  set.seed(3)
  x <- rnorm(200)
  expect_no_error(rootonorm(x, type = "hanging"))
})

test_that("rootonorm works with scale='raw'", {
  pdf(tempfile())
  on.exit(dev.off())
  set.seed(4)
  x <- rnorm(200)
  expect_no_error(rootonorm(x, scale = "raw"))
})

test_that("rootonorm works with scale='sqrt'", {
  pdf(tempfile())
  on.exit(dev.off())
  set.seed(5)
  x <- rnorm(200)
  expect_no_error(rootonorm(x, scale = "sqrt"))
})

test_that("rootonorm accepts custom mu and s arguments", {
  pdf(tempfile())
  on.exit(dev.off())
  set.seed(6)
  x <- rnorm(200)
  expect_no_error(rootonorm(x, mu = 1, s = 2))
})

test_that("rootonorm errors for non-numeric x", {
  pdf(tempfile())
  on.exit(dev.off())
  expect_error(rootonorm("a"), "'x' must be numeric")
})
