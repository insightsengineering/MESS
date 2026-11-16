# Example data from the documentation (Altman)
expo <- c(rep(1, 9), rep(0, 7))
bp1w <- c(137, 120, 141, 137, 140, 144, 134, 123, 142, 139, 134, 136, 151, 147, 137, 149)
bp_base <- c(147, 129, 158, 164, 134, 155, 151, 141, 153, 133, 129, 152, 161, 154, 141, 156)

test_that("documented example returns the expected htest", {
  res <- prepost.test(bp_base, bp1w, expo)

  expect_s3_class(res, "htest")
  expect_named(res, c("statistic", "se", "p.value", "conf.int", "estimate", "method"))
  expect_equal(unname(res$estimate), -7.176388055, tolerance = 1e-8)
  expect_equal(res$se, 2.522606035, tolerance = 1e-8)
  expect_equal(unname(res$statistic), -2.844831082, tolerance = 1e-8)
  expect_equal(unname(res$p.value), 0.004443498039, tolerance = 1e-8)
  expect_equal(as.vector(res$conf.int), c(-12.120605032, -2.232171079), tolerance = 1e-8)
})

test_that("without missing data the estimate is the mean difference of arm-wise predictions", {
  # With every post value observed the augmentation terms vanish and the
  # estimator reduces to mean(ehat1 - ehat0), where ehatk are predictions
  # from a regression of post on baseline within arm k.
  newdata <- data.frame(bp_base = bp_base)
  ehat1 <- predict(lm(bp1w ~ bp_base, subset = expo == 1), newdata)
  ehat0 <- predict(lm(bp1w ~ bp_base, subset = expo == 0), newdata)

  res <- prepost.test(bp_base, bp1w, expo)
  expect_equal(unname(res$estimate), mean(ehat1 - ehat0))
})

test_that("statistic, p-value and confidence interval are internally consistent", {
  res <- prepost.test(bp_base, bp1w, expo, conf.level = 0.9)

  expect_equal(unname(res$statistic), unname(res$estimate) / res$se)
  expect_equal(unname(res$p.value), 2 * pnorm(-abs(unname(res$statistic))))
  expect_equal(
    as.vector(res$conf.int),
    unname(res$estimate) + c(-1, 1) * qnorm(0.95) * res$se
  )
  expect_equal(attr(res$conf.int, "conf.level"), 0.9)
})

test_that("a fixed delta is used instead of the observed treated proportion", {
  res <- prepost.test(bp_base, bp1w, expo, delta = 0.5)
  expect_equal(unname(res$estimate), -7.176388055, tolerance = 1e-8)
  expect_equal(res$se, 2.64499988, tolerance = 1e-8)

  # delta equal to the observed proportion reproduces the default
  expect_equal(prepost.test(bp_base, bp1w, expo, delta = 9 / 16), prepost.test(bp_base, bp1w, expo))
})

test_that("a logical treatment vector gives the same result as 0/1", {
  expect_equal(prepost.test(bp_base, bp1w, expo == 1), prepost.test(bp_base, bp1w, expo))
})

test_that("missing post values are handled with the semiparametric estimator", {
  skip_known_bug("B07")
  post <- bp1w
  post[c(2, 10, 12)] <- NA

  res <- prepost.test(bp_base, post, expo)
  # Reference value computed with the Davidian, Tsiatis & Leon (2005)
  # estimator using the control-arm regression in the control augmentation term.
  expect_equal(unname(res$estimate), -6.640460422, tolerance = 1e-8)
})

test_that("invalid inputs raise informative errors", {
  expect_error(
    prepost.test(bp_base, bp1w, c(rep(1, 6), rep(2, 5), rep(3, 5))),
    "exactly two treatments"
  )
  expect_error(prepost.test(bp_base, bp1w, ifelse(expo == 1, "T", "C")), "treatment must be numeric")
  expect_error(prepost.test(bp_base, bp1w, factor(expo)), "treatment must be numeric")
  expect_error(prepost.test(bp_base, bp1w, expo, conf.level = 2), "'conf.level' must be a single number")
})
