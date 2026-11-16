# MESS::QIC() is checked against geepack::QIC(), which implements the same
# criterion. Always call MESS::QIC explicitly because geepack exports a QIC()
# generic too.
#
# Models are fitted with `data = <pkg>::<dataset>` so the stored call can be
# re-evaluated by QIC's internal refit from any environment (see B09 for the
# case where the data lives in a local variable).

qic_names <- c("QIC", "QICu", "Quasi Lik", "CIC", "params", "QICC")

test_that("QIC for a binomial geeglm matches geepack::QIC", {
  fit <- geepack::geeglm(resp ~ age + smoke, id = id, data = geepack::ohio,
                         family = binomial, corstr = "exchangeable")
  res <- MESS::QIC(fit)

  expect_named(res, qic_names)
  expect_equal(res, geepack::QIC(fit))
  expect_equal(unname(res["params"]), 3)
  expect_equal(unname(res["QIC"]), 1829.482937, tolerance = 1e-8)
})

test_that("QIC for gaussian, poisson and Gamma geeglm fits matches geepack::QIC", {
  gaussian_fit <- geepack::geeglm(Weight ~ Time, id = Pig, data = geepack::dietox,
                                  family = gaussian, corstr = "ar1")
  expect_equal(MESS::QIC(gaussian_fit), geepack::QIC(gaussian_fit))

  poisson_fit <- geepack::geeglm(y ~ trt + lbase, id = subject, data = MASS::epil,
                                 family = poisson, corstr = "exchangeable")
  expect_equal(MESS::QIC(poisson_fit), geepack::QIC(poisson_fit))

  gamma_fit <- geepack::geeglm(Weight ~ Time, id = Pig, data = geepack::dietox,
                               family = Gamma, corstr = "ar1")
  expect_equal(MESS::QIC(gamma_fit), geepack::QIC(gamma_fit))
})

test_that("several models passed through ... give one row per model", {
  fit1 <- geepack::geeglm(resp ~ age + smoke, id = id, data = geepack::ohio,
                          family = binomial, corstr = "exchangeable")
  fit2 <- geepack::geeglm(resp ~ age, id = id, data = geepack::ohio,
                          family = binomial, corstr = "exchangeable")

  res <- MESS::QIC(fit1, model2 = fit2)

  expect_equal(dim(res), c(2, 6))
  expect_equal(colnames(res), qic_names)
  expect_equal(rownames(res), c("fit1", "fit2"))
  expect_equal(res[1, ], geepack::QIC(fit1))
  expect_equal(res[2, ], geepack::QIC(fit2))
})

test_that("comparing models fitted to different numbers of observations warns", {
  fit1 <- geepack::geeglm(resp ~ age, id = id, data = geepack::ohio, family = binomial)
  fit2 <- geepack::geeglm(resp ~ age, id = id, data = geepack::ohio[geepack::ohio$smoke == 1, ],
                          family = binomial)
  expect_warning(
    MESS::QIC(fit1, model2 = fit2),
    "models are not all fitted to the same number of observations"
  )
})

test_that("QIC for a geekin fit equals QIC for the equivalent exchangeable geeglm", {
  # geekin() evaluates geeglm() in the caller's frame, so geepack must be attached (B35)
  withr::local_package("geepack")
  pig <- geepack::dietox$Pig[1:120]
  # A block-diagonal matrix of ones per cluster is equivalent to an
  # exchangeable working correlation
  block <- outer(pig, pig, "==") * 1

  kin_fit <- geekin(Weight ~ Time, id = geepack::dietox$Pig[1:120],
                    data = geepack::dietox[1:120, ], varlist = list(block))
  exch_fit <- geepack::geeglm(Weight ~ Time, id = Pig, data = geepack::dietox[1:120, ],
                              corstr = "exchangeable")

  expect_equal(MESS::QIC(kin_fit), geepack::QIC(exch_fit), tolerance = 1e-6)
})

test_that("an unsupported family raises an error", {
  fit <- geepack::geeglm(Weight ~ Time, id = Pig, data = geepack::dietox)
  fit$family$family <- "unsupported"
  expect_error(MESS::QIC(fit), "distribution not recognized")
})

test_that("non-geeglm objects are rejected", {
  expect_error(MESS::QIC(structure(list(), class = "geekin")), "QIC requires a geekin object")
  expect_error(MESS::QIC(lm(dist ~ speed, data = cars)), class = "error")
})
