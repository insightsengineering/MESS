test_that("power_prop_test with ratio = 1 matches stats::power.prop.test(strict = TRUE)", {
  expect_equal(
    power_prop_test(n = 50, p1 = 0.5, p2 = 0.7)$power,
    power.prop.test(n = 50, p1 = 0.5, p2 = 0.7, strict = TRUE)$power
  )
})

test_that("power_prop_test sample size with ratio = 1 matches stats::power.prop.test", {
  res <- power_prop_test(p1 = 0.5, p2 = 0.7, power = 0.8)
  expect_equal(res$n, power.prop.test(p1 = 0.5, p2 = 0.7, power = 0.8)$n, tolerance = 1e-6)
  expect_identical(res$note, "n is number in *each* group")
  expect_identical(res$method, "Two-sample comparison of proportions power calculation")
})

test_that("power_prop_test sample size for unequal groups (documented example)", {
  res <- power_prop_test(p1 = 0.65, p2 = 0.85, power = 0.8, ratio = 2)

  # Closed form: (z_{1-a/2} sqrt(pbar qbar (1 + r)) + z_{power} sqrt(r p1 q1 + p2 q2))^2 / (r d^2)
  pbar <- (0.65 + 2 * 0.85) / 3
  n1 <- (qnorm(0.975) * sqrt(pbar * (1 - pbar) * 3) +
           qnorm(0.8) * sqrt(2 * 0.65 * 0.35 + 0.85 * 0.15))^2 / (2 * 0.2^2)

  expect_equal(res$n, c(n1, 2 * n1))
  expect_equal(res$n, c(52.06539, 104.13078), tolerance = 1e-6)
  expect_identical(res$note, "n is vector of number in each group")
  expect_match(res$method, "unequal sample sizes")
})

test_that("power_prop_test solves for p1, p2 and ratio consistently", {
  n <- power.prop.test(p1 = 0.5, p2 = 0.7, power = 0.8)$n
  expect_equal(power_prop_test(n = n, p1 = 0.5, power = 0.8)$p2, 0.7, tolerance = 1e-4)
  expect_equal(power_prop_test(n = n, p2 = 0.7, power = 0.8)$p1, 0.5, tolerance = 1e-4)

  res <- power_prop_test(n = 50, p1 = 0.5, p2 = 0.7, power = 0.8, ratio = NULL)
  ratio <- res$n[2] / res$n[1]
  expect_equal(res$n[1], 50)
  expect_equal(power_prop_test(n = 50, p1 = 0.5, p2 = 0.7, ratio = ratio)$power, 0.8, tolerance = 1e-4)
})

test_that("power_prop_test validates its arguments", {
  expect_error(power_prop_test(n = 50, p1 = 0.5, p2 = 0.7, power = 0.8), "exactly one of")
  expect_error(power_prop_test(n = 50, p1 = 0.5, p2 = 0.7, sig.level = 2), "sig.level")
  expect_error(
    power_prop_test(n = 50, p1 = 0.5, p2 = 0.7, ratio = -1),
    "ratio between group sizes must be positive"
  )
})
