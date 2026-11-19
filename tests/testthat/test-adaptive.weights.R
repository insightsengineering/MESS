make_aw_data <- function(n = 50, p = 5, seed = 1) {
  withr::local_seed(seed)
  x <- matrix(rnorm(n * p), nrow = n)
  list(x = x, y = rnorm(n, mean = x[, 1]))
}

univariate_weights <- function(x, y, nu = 1) {
  apply(x, 2, function(xi) 1 / abs(coef(lm(y ~ xi))[[2]])^nu)
}

test_that("multivariate weights are 1/|beta| from the joint lm() fit", {
  d <- make_aw_data()
  res <- adaptive.weights(d$x, d$y)

  expect_named(res, c("weights", "nu"))
  expect_equal(res$nu, 1)
  expect_equal(unname(res$weights), unname(1 / abs(coef(lm(d$y ~ d$x))[-1])))
})

test_that("univariate weights are 1/|beta| from one lm() fit per column", {
  d <- make_aw_data()
  res <- adaptive.weights(d$x, d$y, weight.method = "univariate")

  expect_equal(unname(res$weights), univariate_weights(d$x, d$y))
})

test_that("nu is used as the exponent of the weights", {
  d <- make_aw_data()
  w1 <- adaptive.weights(d$x, d$y, nu = 1)$weights

  expect_equal(adaptive.weights(d$x, d$y, nu = 2)$weights, w1^2)
  expect_equal(unname(adaptive.weights(d$x, d$y, nu = 0)$weights), rep(1, 5))
})

test_that("univariate weights are used with a warning when p >= n", {
  d <- make_aw_data(n = 5, p = 6, seed = 2)

  expect_warning(res <- adaptive.weights(d$x, d$y), "using univariate weight method since p>n")
  expect_equal(unname(res$weights), univariate_weights(d$x, d$y))
})

test_that("an invalid weight.method is an error", {
  d <- make_aw_data()
  expect_error(adaptive.weights(d$x, d$y, weight.method = "bogus"), "should be one of")
})
