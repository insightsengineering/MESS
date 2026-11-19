# With equal group sizes and sd's, power_t_test() must agree with
# stats::power.t.test(strict = TRUE), which serves as the reference.

test_that("power for equal groups matches stats::power.t.test", {
  res <- power_t_test(n = 20, delta = 1, sd = 1)
  ref <- stats::power.t.test(n = 20, delta = 1, sd = 1, strict = TRUE)

  expect_s3_class(res, "power.htest")
  expect_equal(res$power, ref$power)
  expect_equal(res$method, "Two-sample t test power calculation")
  expect_equal(res$note, "n is number in *each* group")
})

test_that("sample size for equal groups matches stats::power.t.test", {
  res <- power_t_test(delta = 1, sd = 1, power = 0.8)
  ref <- stats::power.t.test(delta = 1, sd = 1, power = 0.8, strict = TRUE)
  expect_equal(res$n, ref$n, tolerance = 1e-6)
})

test_that("one-sample, paired and one-sided variants match stats::power.t.test", {
  expect_equal(
    power_t_test(n = 20, delta = 1, type = "one.sample")$power,
    stats::power.t.test(n = 20, delta = 1, type = "one.sample", strict = TRUE)$power
  )
  paired <- power_t_test(n = 20, delta = 1, type = "paired", alternative = "one.sided")
  expect_equal(
    paired$power,
    stats::power.t.test(n = 20, delta = 1, type = "paired", alternative = "one.sided")$power
  )
  expect_match(paired$note, "*pairs*", fixed = TRUE)
})

test_that("strict = FALSE matches the non-strict stats::power.t.test", {
  expect_equal(
    power_t_test(n = 20, delta = 1, strict = FALSE)$power,
    stats::power.t.test(n = 20, delta = 1)$power
  )
  # Under H0 the non-strict power is half the significance level
  expect_equal(power_t_test(n = 20, delta = 0, strict = FALSE)$power, 0.025)
  expect_equal(power_t_test(n = 20, delta = 0, strict = TRUE)$power, 0.05)
})

test_that("unequal group sizes use the documented power formula", {
  # classical df = n1 + n2 - 2 = 28; ncp = sqrt(n / (1 + 1/ratio)) * delta / sd
  res <- power_t_test(n = 10, delta = 1, ratio = 2, df.method = "classical")
  nu <- 28
  ncp <- sqrt(10 / 1.5)
  q <- qt(0.975, nu)
  expected <- pt(q, nu, ncp, lower.tail = FALSE) + pt(-q, nu, ncp)

  expect_equal(res$power, expected)
  expect_equal(res$n, c(10, 20))
  expect_equal(res$method, "Two-sample t test power calculation with unequal sample sizes")
})

test_that("documented examples give the published sample sizes", {
  expect_equal(
    power_t_test(delta = 300, sd = 450, power = 0.8, ratio = 4)$n,
    c(23.37341, 93.49363),
    tolerance = 1e-6
  )
  res <- power_t_test(delta = 300, sd = 450, power = 0.8, sd.ratio = 2)
  expect_equal(res$n, c(89.61929, 89.61929), tolerance = 1e-6)
  expect_equal(res$sd, c(450, 900))
  expect_equal(res$method, "Two-sample t test power calculation with unequal variances")
})

test_that("each NULL parameter is solved so that the target power is reached", {
  target <- 0.8

  delta <- power_t_test(n = 20, sd = 1, power = target)$delta
  expect_equal(power_t_test(n = 20, delta = delta, sd = 1)$power, target, tolerance = 1e-4)

  sd <- power_t_test(n = 20, delta = 1, sd = NULL, power = target)$sd
  expect_equal(power_t_test(n = 20, delta = 1, sd = sd)$power, target, tolerance = 1e-4)

  alpha <- power_t_test(n = 20, delta = 1, sig.level = NULL, power = target)$sig.level
  # uniroot's default tolerance on the sig.level scale is ~1e-4
  expect_equal(power_t_test(n = 20, delta = 1, sig.level = alpha)$power, target, tolerance = 1e-3)

  res <- power_t_test(n = 15, delta = 1, power = target, ratio = NULL)
  expect_equal(res$n[1], 15)
  ratio <- res$n[2] / 15
  expect_gt(ratio, 1)
  expect_equal(power_t_test(n = 15, delta = 1, ratio = ratio)$power, target, tolerance = 1e-4)

  res <- power_t_test(n = 20, delta = 1, power = target, sd.ratio = NULL)
  expect_equal(res$sd[1], 1)
  expect_equal(power_t_test(n = 20, delta = 1, sd.ratio = res$sd[2])$power, target, tolerance = 1e-4)
})

test_that("a negative delta is treated as its absolute value in the two-sided case", {
  expect_equal(power_t_test(n = 20, delta = -1)$delta, 1)
  expect_equal(power_t_test(n = 20, delta = -1)$power, power_t_test(n = 20, delta = 1)$power)
})

test_that("invalid argument combinations raise informative errors", {
  expect_error(
    power_t_test(n = 20, delta = 1, power = 0.8),
    "exactly one of n, delta, sd, power, sig.level, ratio and sd.ratio must be NULL"
  )
  expect_error(
    power_t_test(n = 20, delta = 1, power = 0.8, type = "paired"),
    "exactly one of n, delta, sd, power, and sig.level must be NULL"
  )
  expect_error(power_t_test(n = 20, delta = 1, ratio = 0.5), "cannot be less than 1")
  expect_error(power_t_test(n = 20, delta = 1, sd.ratio = -1), "must be positive")
})
