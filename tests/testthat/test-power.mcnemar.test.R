# Original validated results (kept from the package's original test suite).
test_that("power_mcnemar_test produces validated results", {
  expect_equal(round(power_mcnemar_test(paid = 0.05, psi = 45 / 5, power = .8)$n), 22.0)
  expect_equal(
    round(power_mcnemar_test(paid = 0.05, psi = 45 / 5, power = .8, alternative = "one.sided")$n),
    17.0
  )
  expect_equal(
    round(power_mcnemar_test(144, paid = 0.25, psi = 45 / 25, method = "exact",
                             alternative = "one.sided", sig.level = 0.025)$power, 2),
    0.80
  )
})

test_that("power_mcnemar_test normal approximation matches the closed-form formula", {
  # Connor (1987): power = pnorm((sqrt(n paid) (psi - 1) - z sqrt(psi + 1)) /
  #                             sqrt(psi + 1 - paid (psi - 1)^2))
  n <- 60
  paid <- 0.1
  psi <- 2
  expected <- pnorm((sqrt(n * paid) * (psi - 1) - qnorm(0.975) * sqrt(psi + 1)) /
                      sqrt(psi + 1 - paid * (psi - 1)^2))

  res <- power_mcnemar_test(n = n, paid = paid, psi = psi)
  expect_equal(res$power, expected)
  expect_equal(res$power, 0.28941991, tolerance = 1e-7)
  expect_identical(res$method, "McNemar paired comparison of proportions approximate power calculation")
  expect_identical(res$note, "n is number of pairs")
})

test_that("power_mcnemar_test cond.exact equals binomial power for the expected discordant count", {
  # Number of discordant pairs: ceiling(n * paid * (1 + psi)) = 18;
  # under H1 the split is binomial with p = 1 / (1 + psi)
  bc <- 18
  expected <- pbinom(qbinom(0.025, bc, 0.5) - 1, bc, 1 / 3) +
    pbinom(qbinom(0.975, bc, 0.5), bc, 1 / 3, lower.tail = FALSE)

  res <- power_mcnemar_test(n = 60, paid = 0.1, psi = 2, method = "cond.exact")
  expect_equal(res$power, expected)
  expect_equal(res$power, 0.23121728, tolerance = 1e-7)
  expect_match(res$method, "exact conditional")
})

test_that("power_mcnemar_test exact unconditional power", {
  res <- power_mcnemar_test(n = 60, paid = 0.1, psi = 2, method = "exact")
  expect_equal(res$power, 0.28215995, tolerance = 1e-7)
  expect_match(res$method, "exact unconditional")
})

test_that("power_mcnemar_test solves for paid, psi and sig.level", {
  power <- power_mcnemar_test(n = 60, paid = 0.1, psi = 2)$power

  expect_equal(power_mcnemar_test(n = 60, psi = 2, power = power)$paid, 0.1, tolerance = 1e-4)
  expect_equal(power_mcnemar_test(n = 60, paid = 0.1, power = power)$psi, 2, tolerance = 1e-4)
  expect_equal(
    power_mcnemar_test(n = 60, paid = 0.1, psi = 2, power = power, sig.level = NULL)$sig.level,
    0.05,
    tolerance = 1e-4
  )
})

test_that("power_mcnemar_test validates its arguments", {
  expect_error(power_mcnemar_test(n = 60, paid = .1, psi = 2, power = .8), "exactly one of")
  expect_error(power_mcnemar_test(n = 60, paid = .6, psi = 2), "paid is the smallest discordant probability")
  expect_error(power_mcnemar_test(n = 60, paid = .1, psi = 0.5), "psi must be 1 or greater")
  expect_error(power_mcnemar_test(n = 60, paid = .4, psi = 3), "psi cannot be so big")
})

test_that("power_mcnemar_test accepts psi = 1 as its error message states", {
  skip_known_bug("B34")
  # With no effect, two-sided normal-approximation power is sig.level / 2
  expect_equal(power_mcnemar_test(n = 60, paid = 0.1, psi = 1)$power, 0.025)
})
