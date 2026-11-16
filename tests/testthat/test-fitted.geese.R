# fitted.geese() should return the inverse-link of the linear predictor, which
# is what fitted() gives for the equivalent geeglm fit.

test_that("fitted.geese returns fitted values for a binomial geese fit", {
  skip_known_bug("B12")
  dat <- make_gee_data()
  fit <- geepack::geese(y ~ x1 + age, id = id, data = dat, family = binomial, corstr = "exchangeable")
  ref <- geepack::geeglm(y ~ x1 + age, id = id, data = dat, family = binomial, corstr = "exchangeable")

  expect_equal(as.vector(fitted(fit)), as.vector(fitted(ref)))
})

test_that("fitted.geese returns fitted values for a gaussian geese fit", {
  skip_known_bug("B12")
  dat <- make_gee_data()
  fit <- geepack::geese(x1 ~ age, id = id, data = dat, corstr = "exchangeable")

  X <- model.matrix(~ age, data = dat)
  expect_equal(as.vector(fitted(fit)), as.vector(X %*% fit$beta))
})
