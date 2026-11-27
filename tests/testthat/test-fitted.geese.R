test_that("fitted.geese returns fitted values for a binomial geese fit", {
  dat <- make_gee_data()
  fit <- geepack::geese(y ~ x1 + age, id = id, data = dat, corstr = "exchangeable", family = binomial())
  ref <- geepack::geeglm(y ~ x1 + age, id = id, data = dat, corstr = "exchangeable", family = binomial())

  expect_equal(as.vector(fitted(fit)), as.vector(fitted(ref)))
})

test_that("fitted.geese returns fitted values for a gaussian geese fit", {
  dat <- make_gee_data()
  fit <- geepack::geese(x1 ~ age, id = id, data = dat, corstr = "exchangeable", family = gaussian())
  ref <- geepack::geeglm(x1 ~ age, id = id, data = dat, corstr = "exchangeable", family = gaussian())

  expect_equal(as.vector(fitted(fit)), as.vector(fitted(ref)))
})

test_that("fitted.geese returns fitted values for a default geese fit", {
  dat <- make_gee_data()
  fit <- geepack::geese(x1 ~ age, id = id, data = dat, corstr = "exchangeable")
  ref <- geepack::geeglm(x1 ~ age, id = id, data = dat, corstr = "exchangeable")

  expect_equal(as.vector(fitted(fit)), as.vector(fitted(ref)))
})

test_that("fitted.geese returns fitted values for a default geese fit", {
  dat <- make_gee_data()
  fit <- geepack::geese(x1 ~ age, id = id, data = dat, corstr = "exchangeable")
  ref <- geepack::geeglm(x1 ~ age, id = id, data = dat, corstr = "exchangeable")

  expect_equal(as.vector(fitted(fit)), as.vector(fitted(ref)))
})
