# With x = I and y = (3, 2, 1), |x'y| = (3, 2, 1) and lambdamax = 3.
# The global strong rule discards variable j when |x_j'y| < 2 * lambda - lambdamax.

test_that("global strong rule returns the indices with |x'y| below the criterion", {
  res <- screen_variables(diag(3), c(3, 2, 1), lambda = c(2, 2.5, 3))

  expect_named(res, c("lambda", "selected", "method"))
  expect_equal(res$lambda, c(2, 2.5, 3))
  expect_equal(res$method, "global-strong")
  # criteria 1, 2, 3
  expect_equal(res$selected, list(integer(0), 3L, 2:3))
})

test_that("the screening agrees with the rule on random data", {
  withr::local_seed(10)
  x <- matrix(rnorm(30 * 10), nrow = 30)
  y <- rnorm(30, mean = x[, 1])
  score <- abs(drop(crossprod(x, y)))
  lambda <- max(score) * c(0.6, 0.9)

  res <- screen_variables(x, y, lambda = lambda)
  expect_equal(res$selected, lapply(lambda, function(l) which(score < 2 * l - max(score))))
})

test_that("a table is accepted as x", {
  tbl <- as.table(diag(3))
  expect_equal(
    screen_variables(tbl, c(3, 2, 1), lambda = c(2.5, 3))$selected,
    list(3L, 2:3)
  )
})

test_that("selected is a list even when every lambda selects the same number of variables", {
  skip_known_bug("B40")
  res <- screen_variables(diag(3), c(3, 2, 1), lambda = c(2.4, 2.5))
  expect_equal(res$selected, list(3L, 3L))
})

test_that("global-DPP method applies the DPP criterion", {
  skip_known_bug("B08")
  x <- diag(3)
  y <- c(3, 2, 1)
  lambda <- 2.5
  # criterion = lambdamax - ||x||_2 * ||y||_2 * (lambdamax - lambda) / lambda
  criterion <- 3 - 1 * sqrt(14) * 0.5 / 2.5

  res <- screen_variables(x, y, lambda = lambda, method = "global-DPP")
  expect_equal(res$method, "global-DPP")
  expect_equal(res$selected, list(which(c(3, 2, 1) < criterion)))
})

test_that("invalid inputs raise informative errors", {
  x <- diag(3)
  y <- c(3, 2, 1)
  expect_error(screen_variables(as.data.frame(x), y), "needs matrix or table as input")
  expect_error(screen_variables(as.vector(x), y), "needs matrix or table as input")
  expect_error(screen_variables(x, y, lambda = 0), "lambda must be positive")
  expect_error(screen_variables(x, y, lambda = c(0.1, -1)), "lambda must be positive")
  expect_error(screen_variables(x, y, method = "unknown"), "'arg' should be one of")
})
