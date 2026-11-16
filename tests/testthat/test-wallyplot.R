test_that("wallyplot.default works for a vector x and y with hide=TRUE", {
  pdf(tempfile())
  on.exit(dev.off())
  set.seed(1)
  expect_no_error(wallyplot(1:20, rnorm(20)))
})

test_that("wallyplot.default returns invisible NULL", {
  pdf(tempfile())
  on.exit(dev.off())
  set.seed(1)
  result <- wallyplot(1:20, rnorm(20))
  expect_null(result)
})

test_that("wallyplot.default works with hide=FALSE", {
  pdf(tempfile())
  on.exit(dev.off())
  set.seed(2)
  expect_no_error(wallyplot(1:20, rnorm(20), hide = FALSE))
})

test_that("wallyplot.lm works for an lm object", {
  pdf(tempfile())
  on.exit(dev.off())
  set.seed(3)
  model <- lm(Volume ~ Girth + Height, data = trees)
  expect_no_error(wallyplot(model))
})

test_that("wallyplot.lm returns invisible NULL", {
  pdf(tempfile())
  on.exit(dev.off())
  set.seed(3)
  model <- lm(Volume ~ Girth + Height, data = trees)
  result <- wallyplot(model)
  expect_null(result)
})

test_that("wallyplot errors when y matrix does not have 9 columns", {
  pdf(tempfile())
  on.exit(dev.off())
  expect_error(wallyplot(1:20, matrix(rnorm(20 * 5), ncol = 5)))
})

test_that("wallyplot errors when x and y lengths are mismatched", {
  pdf(tempfile())
  on.exit(dev.off())
  expect_error(wallyplot(1:20, rnorm(15)))
})
