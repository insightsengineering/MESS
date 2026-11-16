context("Test power_prop_test")

test_that("documented example gives the expected result", {
    result <- power_prop_test(n=NULL, p1=.65, p2=.85, power=.8, ratio=2)
    expect_equal(result$n, c(52.06539, 104.13078), tolerance=1e-4)
    expect_match(result$method, "unequal sample sizes")
})

test_that("solving for power given n, p1, p2, sig.level", {
    result <- power_prop_test(n=50, p1=.5, p2=.7, sig.level=0.05, power=NULL)
    expect_equal(result$power, 0.5331062, tolerance=1e-5)
})

test_that("solving for p2 given n, p1, power, sig.level", {
    base_power <- power_prop_test(n=50, p1=.5, p2=.7, sig.level=0.05, power=NULL)$power
    result <- power_prop_test(n=50, p1=.5, p2=NULL, sig.level=0.05, power=base_power)
    expect_equal(result$p2, 0.7, tolerance=1e-3)
})

test_that("solving for p1 given n, p2, power, sig.level", {
    base_power <- power_prop_test(n=50, p1=.5, p2=.7, sig.level=0.05, power=NULL)$power
    result <- power_prop_test(n=50, p1=NULL, p2=.7, sig.level=0.05, power=base_power)
    expect_equal(result$p1, 0.5, tolerance=1e-3)
})

test_that("solving for ratio given n, p1, p2, power", {
    result <- power_prop_test(n=50, p1=.5, p2=.7, sig.level=0.05, power=.8, ratio=NULL)
    expect_equal(result$n[1], 50)
    expect_equal(result$n[2], 398.4383, tolerance=1e-3)
})

test_that("alternative='one.sided' gives higher power than the two-sided test", {
    one_sided <- power_prop_test(n=50, p1=.5, p2=.7, sig.level=0.05, power=NULL, alternative="one.sided")
    two_sided <- power_prop_test(n=50, p1=.5, p2=.7, sig.level=0.05, power=NULL, alternative="two.sided")
    expect_equal(one_sided$power, 0.6573125, tolerance=1e-5)
    expect_true(one_sided$power > two_sided$power)
})

test_that("errors when not exactly one of the mutually exclusive arguments is NULL", {
    expect_error(power_prop_test(n=50, p1=.5, p2=.7, sig.level=0.05, power=.8),
                 "exactly one of")
})

test_that("errors for a nonsensical sig.level or ratio", {
    expect_error(power_prop_test(n=50, p1=.5, p2=.7, sig.level=2, power=NULL),
                 "sig.level")
    expect_error(power_prop_test(n=50, p1=.5, p2=.7, sig.level=0.05, power=NULL, ratio=-1),
                 "ratio between group sizes must be positive")
})
