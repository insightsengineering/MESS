skip("Known bugs documented in this file")
# Tests documenting confirmed defects in the package source.
#
# Every test here starts with # behaviour. When a defect is fixed upstream, delete the skip line and move the
# test back to the test file for that function (the section headers below).
# See tests/testthat/KNOWN_BUGS.md for details on each defect.

# test-drop1.geeglm.R ---------------------------------------------------------

test_that("drop1() score tests work when the data is a local variable", {
    local_data <- warpbreaks
    fit <- geepack::geeglm(breaks ~ wool + tension, id = rep(1:18, each = 3), data = local_data, family = poisson)
    expect_named(drop1(fit, test = "score"), c("DF", "Score", "Pr(>Chi)"))
})

test_that("drop1() on geeglm with test = 'none' keeps the statistic", {
    out <- drop1(fit_warpbreaks_geeglm(), test = "none")
  expect_named(out, c("DF", "Wald"))
})

test_that("drop1() on geem computes score tests", {
    g <- geeM::geem(breaks ~ wool + tension, id = rep(1:18, each = 3), data = warpbreaks, family = "poisson")
  expect_named(drop1(g, test = "score"), c("DF", "Score", "Pr(>Chi)"))
})

test_that("drop1() on geese gives Wald tests per term", {
    gs <- geepack::geese(
    breaks ~ wool + tension, id = rep(1:18, each = 3), data = warpbreaks,
    family = poisson, corstr = "exchangeable"
  )
  expect_equal(drop1(gs)$Wald, drop1(fit_warpbreaks_geeglm())$Wald)
})

# test-extended.shared.R ------------------------------------------------------

test_that("extended.shared() handles a pedigree with a single non-founder", {
    trio <- kinship2::pedigree(1:3, c(0, 0, 1), c(0, 0, 2), c(1, 2, 1))
  expect_equal(extended.shared(trio), nuclear_expected(1, 1)[1:3, 1:3])
})

test_that("extended.shared() passes rho and theta on for a pedigreeList", {
    result <- extended.shared(make_two_families(), rho = 0.5, theta = 0.5)
  expect_equal(as.matrix(result)[1:4, 1:4], nuclear_expected(0.5, 0.5), ignore_attr = TRUE)
})

# test-feature.test.R ---------------------------------------------------------

test_that("feature.test selects the true predictor and returns permutation p-values", {
    d <- make_feature_data()
  withr::local_seed(1)

  capture.output(res <- feature.test(d$x, d$y, B = 5, s = 0.2, control = list(maxcores = 1)))

  expect_identical(res$lasso.selected[1], 1L)
  expect_identical(res$ols.selected[1], 1L)
  expect_true(all(c(res$p.full, res$p.maxols, res$p.maxlasso) >= 0))
  expect_true(all(c(res$p.full, res$p.maxols, res$p.maxlasso) <= 1))
})

# test-fitted.geese.R ---------------------------------------------------------

# fitted.geese() should return the inverse-link of the linear predictor, which
# is what fitted() gives for the equivalent geeglm fit.

test_that("fitted.geese returns fitted values for a binomial geese fit", {
    dat <- make_gee_data()
  fit <- geepack::geese(y ~ x1 + age, id = id, data = dat, family = binomial, corstr = "exchangeable")
  ref <- geepack::geeglm(y ~ x1 + age, id = id, data = dat, family = binomial, corstr = "exchangeable")

  expect_equal(as.vector(fitted(fit)), as.vector(fitted(ref)))
})

test_that("fitted.geese returns fitted values for a gaussian geese fit", {
    dat <- make_gee_data()
  fit <- geepack::geese(x1 ~ age, id = id, data = dat, corstr = "exchangeable")

  X <- model.matrix(~ age, data = dat)
  expect_equal(as.vector(fitted(fit)), as.vector(X %*% fit$beta))
})

# test-geekin.R ---------------------------------------------------------------

test_that("print.geekin does not change the global digits option", {
    dat <- make_gee_data()
  V <- exchangeable_varlist()
  fit <- geekin(y ~ x1 + age, id = dat$id, data = dat, family = binomial, varlist = list(V))

  withr::local_options(digits = 7)
  capture.output(print(fit, digits = 3))
  expect_identical(getOption("digits"), 7L)
})

test_that("geekin works when geepack is not attached", {
  env <- new.env(parent = globalenv())
  env$dat <- make_gee_data()
  env$V <- exchangeable_varlist()

  expect_no_error(evalq(
    MESS::geekin(y ~ x1 + age, id = dat$id, data = dat, family = binomial, varlist = list(V)),
    env
  ))
})

# test-loadRData.R ------------------------------------------------------------

test_that("loadRData returns an object that is named 'filename'", {
  path <- withr::local_tempfile(fileext = ".RData")
  filename <- 1:3
  save(filename, file = path)

  expect_identical(loadRData(path), 1:3)
})

# test-lower.tri.vector.R -----------------------------------------------------

test_that("lower.tri.vector rejects a non-square matrix", {
  expect_error(lower.tri.vector(matrix(1:12, nrow = 3)), "square matrix")
})

# test-panel.R ----------------------------------------------------------------

test_that("panel.hist and panel.r2 restore par('usr') without warnings", {
  withr::local_pdf(NULL)
  x <- c(1, 3, 2, 5, 4, 6, 8, 7, 9)

  plot.new()
  plot.window(xlim = c(0, 10), ylim = c(0, 10))
  usr <- par("usr")
  expect_no_warning(panel.hist(x))
  expect_equal(par("usr"), usr)
  expect_no_warning(panel.r2(x, rev(x)))
  expect_equal(par("usr"), usr)
})

# test-power.binom.test.R -----------------------------------------------------

test_that("power_binom_test two-sided test keeps its level for symmetric p0", {
  expect_lte(power_binom_test(n = 50, p0 = 0.5, pa = 0.5)$power, 0.05)
  expect_equal(power_binom_test(n = 50, p0 = 0.5, pa = 0.75)$power, binom_test_power(50, 0.5, 0.75))
})

test_that("power_binom_test solves for p0", {
  res <- power_binom_test(n = 50, pa = 0.45, power = 0.8, alternative = "greater")
  expect_equal(
    power_binom_test(n = 50, p0 = res$p0, pa = 0.45, alternative = "greater")$power,
    0.8,
    tolerance = 1e-3
  )
})

# test-power.mcnemar.test.R ---------------------------------------------------

test_that("power_mcnemar_test accepts psi = 1 as its error message states", {
  # With no effect, two-sided normal-approximation power is sig.level / 2
  expect_equal(power_mcnemar_test(n = 60, paid = 0.1, psi = 1)$power, 0.025)
})

# test-power.prop.test.R ------------------------------------------------------

test_that("power_prop_test one-sided power matches stats::power.prop.test", {
  expect_equal(power_prop_test(n = 50, p1 = 0.5, p2 = 0.5, alternative = "one.sided")$power, 0.05)
  expect_equal(
    power_prop_test(n = 50, p1 = 0.5, p2 = 0.7, alternative = "one.sided")$power,
    power.prop.test(n = 50, p1 = 0.5, p2 = 0.7, alternative = "one.sided")$power
  )
})

test_that("power_prop_test solves for sig.level", {
  res <- power_prop_test(n = 93, p1 = 0.5, p2 = 0.7, power = 0.8, sig.level = NULL)
  expect_equal(
    res$sig.level,
    power.prop.test(n = 93, p1 = 0.5, p2 = 0.7, power = 0.8, sig.level = NULL)$sig.level,
    tolerance = 1e-3
  )
})

# test-prepost.R --------------------------------------------------------------

test_that("missing post values are handled with the semiparametric estimator", {
  # Example data from the documentation (Altman)
  expo <- c(rep(1, 9), rep(0, 7))
  bp_base <- c(147, 129, 158, 164, 134, 155, 151, 141, 153, 133, 129, 152, 161, 154, 141, 156)
  post <- c(137, 120, 141, 137, 140, 144, 134, 123, 142, 139, 134, 136, 151, 147, 137, 149)
  post[c(2, 10, 12)] <- NA

  res <- prepost.test(bp_base, post, expo)
  # Reference value computed with the Davidian, Tsiatis & Leon (2005)
  # estimator using the control-arm regression in the control augmentation term.
  expect_equal(unname(res$estimate), -6.640460422, tolerance = 1e-8)
})

# test-qic.R ------------------------------------------------------------------

test_that("QIC works when the model data is a local variable", {
  dat <- geepack::ohio
  fit <- geepack::geeglm(resp ~ age + smoke, id = id, data = dat,
                         family = binomial, corstr = "exchangeable")
  expect_equal(MESS::QIC(fit), geepack::QIC(fit))
})

test_that("models can be compared by passing them positionally", {
  fit1 <- geepack::geeglm(resp ~ age + smoke, id = id, data = geepack::ohio, family = binomial)
  fit2 <- geepack::geeglm(resp ~ age, id = id, data = geepack::ohio, family = binomial)

  res <- MESS::QIC(fit1, fit2)
  expect_equal(dim(res), c(2, 6))
})

test_that("QIC works for an ordgee fit", {
  fit <- geepack::ordgee(ordered(resp) ~ age, id = id, data = geepack::ohio)
  expect_named(MESS::QIC(fit), c("QIC", "QICu", "Quasi Lik", "CIC", "params", "QICC"))
})

# test-RcppExports-stats.R ----------------------------------------------------

test_that("cumsumbinning() numbers groups from 1 when the first value exceeds the threshold", {
  expect_equal(cumsumbinning(c(15, 1, 2), 10), c(1, 2, 2))
})

test_that("ks_cumtest() rescales prob to sum to 1", {
  withr::local_seed(1)
  res <- ks_cumtest(1:6, B = 100, prob = c(5, 1, 1, 1, 1, 1))
  expect_equal(unname(res$statistic), 6 / 10 - 3 / 21)
})

test_that("maximum_subarray() rejects an empty vector", {
  expect_error(maximum_subarray(numeric(0)))
})

# test-rnormperfect.R ---------------------------------------------------------

test_that("the sample is centred at mean and scaled by sd", {
  res <- rnorm_perfect(30, mean = 10, sd = 2)
  expect_equal(mean(res), 10)
  expect_equal(res, 10 + 2 * qnorm((1:30) / 31))
})

# test-screener.R -------------------------------------------------------------

test_that("selected is a list even when every lambda selects the same number of variables", {
  res <- screen_variables(diag(3), c(3, 2, 1), lambda = c(2.4, 2.5))
  expect_equal(res$selected, list(3L, 3L))
})

test_that("global-DPP method applies the DPP criterion", {
  x <- diag(3)
  y <- c(3, 2, 1)
  lambda <- 2.5
  # criterion = lambdamax - ||x||_2 * ||y||_2 * (lambdamax - lambda) / lambda
  criterion <- 3 - 1 * sqrt(14) * 0.5 / 2.5

  res <- screen_variables(x, y, lambda = lambda, method = "global-DPP")
  expect_equal(res$method, "global-DPP")
  expect_equal(res$selected, list(which(c(3, 2, 1) < criterion)))
})
