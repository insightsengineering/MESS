context("Test power_binom_test")

test_that("documented examples give the expected results", {
    r1 <- power_binom_test(n = 50, p0 = .50, pa = .75)
    expect_equal(r1$power, 0.971, tolerance=1e-3)

    r2 <- power_binom_test(p0 = .50, pa = .75, power = .90)
    expect_equal(r2$n, 41, tolerance=1e-2)

    r3 <- power_binom_test(n = 50, p0 = .25, power = .90, alternative="less")
    expect_equal(r3$pa, 0.0954, tolerance=1e-3)
})

test_that("alternative='greater' computes the expected power and solves for pa", {
    power_result <- power_binom_test(n=50, p0=.25, pa=.45, alternative="greater")
    expect_equal(power_result$power, 0.8726549, tolerance=1e-5)

    pa_result <- power_binom_test(n=50, p0=.25, power=power_result$power, alternative="greater")
    expect_equal(pa_result$pa, 0.45, tolerance=1e-2)
})

test_that("solving for sig.level given n, p0, pa, power (alternative='less')", {
    base <- power_binom_test(n=50, p0=.25, power=.90, alternative="less")
    recovered <- power_binom_test(n=50, p0=.25, pa=base$pa, power=.90, sig.level=NULL, alternative="less")
    expect_equal(recovered$sig.level, 0.05, tolerance=0.02)
})

test_that("solving for sig.level given n, p0, pa, power (alternative='greater')", {
    power_result <- power_binom_test(n=50, p0=.25, pa=.45, sig.level=0.05, alternative="greater")
    recovered <- power_binom_test(n=50, p0=.25, pa=.45, power=power_result$power, sig.level=NULL, alternative="greater")
    expect_equal(recovered$sig.level, 0.05, tolerance=0.02)
})

test_that("errors when not exactly one of the mutually exclusive arguments is NULL", {
    expect_error(power_binom_test(n=50, p0=.5, pa=.75, power=.9),
                 "exactly one of")
})

test_that("errors for a nonsensical sig.level", {
    expect_error(power_binom_test(n=50, p0=.5, pa=.75, power=NULL, sig.level=2),
                 "sig.level")
})

test_that("errors when solving for p0, pa, or sig.level with a two-sided alternative", {
    expect_error(power_binom_test(n=50, p0=.5, pa=NULL, power=.9, sig.level=0.05, alternative="two.sided"),
                 "symmetry")
    expect_error(power_binom_test(n=50, p0=NULL, pa=.75, power=.9, sig.level=0.05, alternative="two.sided"),
                 "symmetry")
    expect_error(power_binom_test(n=50, p0=.5, pa=.75, power=.9, sig.level=NULL, alternative="two.sided"),
                 "symmetry")
})
