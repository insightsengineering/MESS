test_that("mfastLmCpp matches marginal simple regressions fitted with lm", {
  withr::local_seed(1)
  y <- rnorm(10)
  x <- matrix(rnorm(30), ncol = 3)

  res <- mfastLmCpp(y, x)
  expect_s3_class(res, "data.frame")
  expect_named(res, c("coefficients", "stderr", "tstat"))

  expected <- t(sapply(1:3, function(j) coef(summary(lm(y ~ x[, j])))[2, 1:3]))
  expect_equal(unname(as.matrix(res)), unname(expected))
})

test_that("mfastLmCpp without intercept matches regressions through the origin", {
  withr::local_seed(1)
  y <- rnorm(10)
  x <- matrix(rnorm(30), ncol = 3)

  res <- mfastLmCpp(y, x, addintercept = FALSE)
  expected <- t(sapply(1:3, function(j) coef(summary(lm(y ~ x[, j] - 1)))[1, 1:3]))
  expect_equal(unname(as.matrix(res)), unname(expected))
})

test_that("plr on a vector outcome equals mfastLmCpp", {
  withr::local_seed(42)
  y <- rnorm(50)
  x <- matrix(rnorm(150), ncol = 3)

  expect_equal(plr(y, x), mfastLmCpp(y = y, x = x))
  expect_equal(plr(y, x, addintercept = FALSE), mfastLmCpp(y = y, x = x, addintercept = FALSE))
})

test_that("plr on a matrix outcome applies plr to each column", {
  withr::local_seed(42)
  y <- matrix(rnorm(200), ncol = 4)
  x <- matrix(rnorm(150), ncol = 3)

  res <- plr(y, x)
  expect_type(res, "list")
  expect_length(res, 4)
  for (k in 1:4) expect_equal(res[[k]], plr(y[, k], x))
})
