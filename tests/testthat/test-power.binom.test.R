# Reference power for the two-sided test: probability under pa of the exact
# rejection region of stats::binom.test() at level 0.05.
binom_test_power <- function(n, p0, pa) {
  x <- 0:n
  reject <- vapply(x, function(k) binom.test(k, n, p0)$p.value <= 0.05, logical(1))
  sum(dbinom(x[reject], n, pa))
}

test_that("power_binom_test two-sided power matches the binom.test rejection region", {
  expect_equal(power_binom_test(n = 50, p0 = 0.3, pa = 0.5)$power, binom_test_power(50, 0.3, 0.5))
  expect_equal(power_binom_test(n = 40, p0 = 0.3, pa = 0.1)$power, binom_test_power(40, 0.3, 0.1))
  expect_equal(power_binom_test(n = 20, p0 = 0.5, pa = 0.75)$power, binom_test_power(20, 0.5, 0.75))
})

test_that("power_binom_test two-sided test keeps its level for symmetric p0", {
  skip_known_bug("B30")
  expect_lte(power_binom_test(n = 50, p0 = 0.5, pa = 0.5)$power, 0.05)
  expect_equal(power_binom_test(n = 50, p0 = 0.5, pa = 0.75)$power, binom_test_power(50, 0.5, 0.75))
})

test_that("power_binom_test one-sided power matches exact binomial tail probabilities", {
  # greater: reject when X > qbinom(0.95, n, p0)
  expect_equal(
    power_binom_test(n = 50, p0 = 0.25, pa = 0.45, alternative = "greater")$power,
    pbinom(qbinom(0.95, 50, 0.25), 50, 0.45, lower.tail = FALSE)
  )
  # less: reject when X < qbinom(0.05, n, p0)
  expect_equal(
    power_binom_test(n = 50, p0 = 0.25, pa = 0.10, alternative = "less")$power,
    pbinom(qbinom(0.05, 50, 0.25) - 1, 50, 0.10)
  )
})

test_that("power_binom_test is vectorised over n", {
  expect_equal(
    power_binom_test(n = c(40, 50), p0 = 0.3, pa = 0.5)$power,
    c(binom_test_power(40, 0.3, 0.5), binom_test_power(50, 0.3, 0.5))
  )
})

test_that("power_binom_test solves for n giving at least the requested power", {
  # Exact binomial power is not monotone in n, so only check that the
  # returned n achieves the requested power.
  res <- power_binom_test(p0 = 0.3, pa = 0.5, power = 0.8)
  expect_s3_class(res, "power.htest")
  expect_gte(power_binom_test(n = ceiling(res$n), p0 = 0.3, pa = 0.5)$power, 0.8)
})

test_that("power_binom_test solves for pa (documented example)", {
  res <- power_binom_test(n = 50, p0 = 0.25, power = 0.90, alternative = "less")
  expect_equal(res$pa, 0.0954, tolerance = 1e-3)
  expect_equal(
    power_binom_test(n = 50, p0 = 0.25, pa = res$pa, alternative = "less")$power,
    0.90,
    tolerance = 1e-4
  )
})

test_that("power_binom_test solves for p0", {
  skip_known_bug("B31")
  res <- power_binom_test(n = 50, pa = 0.45, power = 0.8, alternative = "greater")
  expect_equal(
    power_binom_test(n = 50, p0 = res$p0, pa = 0.45, alternative = "greater")$power,
    0.8,
    tolerance = 1e-3
  )
})

test_that("power_binom_test validates its arguments", {
  expect_error(power_binom_test(n = 50, p0 = 0.5, pa = 0.75, power = 0.9), "exactly one of")
  expect_error(power_binom_test(n = 50, p0 = 0.5, pa = 0.75, sig.level = 2), "sig.level")
  expect_error(power_binom_test(n = 50, p0 = 0.5, power = 0.9), "symmetry")
  expect_error(power_binom_test(n = 50, pa = 0.75, power = 0.9), "symmetry")
  expect_error(
    power_binom_test(n = 50, p0 = 0.5, pa = 0.75, power = 0.9, sig.level = NULL),
    "symmetry"
  )
})
