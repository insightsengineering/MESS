context("Test power_t_test")

test_that("documented examples give the expected results", {
    r1 <- power_t_test(delta=300, sd=450, power=.8, ratio=4)
    expect_equal(r1$n, c(23.37341, 93.49363), tolerance=1e-4)

    r2 <- power_t_test(delta=300, sd=450, power=.8, sd.ratio=2)
    expect_equal(r2$n, c(89.61929, 89.61929), tolerance=1e-4)

    r3 <- power_t_test(n=50, delta=300, sd=450, power=.8, ratio=NULL, sd.ratio=2)
    expect_equal(r3$n, c(50.0000, 111.3118), tolerance=1e-3)
})

test_that("solving for power given n, delta, sd, sig.level", {
    result <- power_t_test(n=20, delta=1, sd=1, sig.level=0.05, power=NULL)
    expect_equal(result$power, 0.8689528, tolerance=1e-5)
})

test_that("solving for n given delta, sd, power, sig.level", {
    result <- power_t_test(n=NULL, delta=1, sd=1, sig.level=0.05, power=0.8)
    expect_equal(result$n, 16.71473, tolerance=1e-4)
})

test_that("solving for delta given n, sd, power, sig.level", {
    base_power <- power_t_test(n=20, delta=1, sd=1, sig.level=0.05, power=NULL)$power
    result <- power_t_test(n=20, delta=NULL, sd=1, sig.level=0.05, power=base_power)
    expect_equal(result$delta, 1, tolerance=1e-4)
})

test_that("solving for sd given n, delta, power, sig.level", {
    base_power <- power_t_test(n=20, delta=1, sd=1, sig.level=0.05, power=NULL)$power
    result <- power_t_test(n=20, delta=1, sd=NULL, sig.level=0.05, power=base_power)
    expect_equal(result$sd, 1, tolerance=1e-4)
})

test_that("solving for sig.level given n, delta, sd, power", {
    base_power <- power_t_test(n=20, delta=1, sd=1, sig.level=0.05, power=NULL)$power
    result <- power_t_test(n=20, delta=1, sd=1, sig.level=NULL, power=base_power)
    expect_equal(result$sig.level, 0.05, tolerance=1e-4)
})

test_that("solving for ratio (group size ratio) given n, delta, sd, power", {
    result <- power_t_test(n=20, delta=1, sd=1, power=0.8, sig.level=0.05, ratio=NULL)
    expect_equal(result$n[1], 20)
    expect_equal(result$n[2], 14.47891, tolerance=1e-4)
})

test_that("solving for sd.ratio given n, delta, sd, power", {
    result <- power_t_test(n=20, delta=1, sd=1, power=0.8, sig.level=0.05, sd.ratio=NULL)
    expect_equal(result$sd[1], 1)
    expect_equal(result$sd[2], 1.189961, tolerance=1e-4)
})

test_that("round trip: power -> delta recovers the original delta", {
    p <- power_t_test(n=30, delta=2, sd=3, sig.level=0.05, power=NULL)$power
    back <- power_t_test(n=30, delta=NULL, sd=3, sig.level=0.05, power=p)
    expect_equal(back$delta, 2, tolerance=1e-4)
})

test_that("type variants (one.sample, paired, two.sample) behave as documented", {
    one_sample <- power_t_test(n=20, delta=1, sd=1, sig.level=0.05, type="one.sample")
    expect_equal(one_sample$power, 0.9885913, tolerance=1e-5)

    paired <- power_t_test(n=20, delta=1, sd=1, sig.level=0.05, type="paired")
    expect_equal(paired$power, 0.9885913, tolerance=1e-5)
    expect_match(paired$note, "pairs")

    two_sample <- power_t_test(n=20, delta=1, sd=1, sig.level=0.05, type="two.sample")
    expect_equal(two_sample$power, 0.8689528, tolerance=1e-5)
})

test_that("alternative='one.sided' gives higher power than the two-sided test", {
    one_sided <- power_t_test(n=20, delta=1, sd=1, sig.level=0.05, alternative="one.sided")
    two_sided <- power_t_test(n=20, delta=1, sd=1, sig.level=0.05, alternative="two.sided")
    expect_equal(one_sided$power, 0.9279025, tolerance=1e-5)
    expect_true(one_sided$power > two_sided$power)
})

test_that("df.method welch and classical agree with equal n and equal sd", {
    welch <- power_t_test(n=20, delta=1, sd=1, sig.level=0.05, df.method="welch")
    classical <- power_t_test(n=20, delta=1, sd=1, sig.level=0.05, df.method="classical")
    expect_equal(welch$power, 0.8689528, tolerance=1e-5)
    expect_equal(classical$power, welch$power, tolerance=1e-4)
})

test_that("strict=FALSE changes power compared to strict=TRUE when delta=0", {
    non_strict <- power_t_test(n=20, delta=0, sd=1, sig.level=0.05, strict=FALSE)
    strict <- power_t_test(n=20, delta=0, sd=1, sig.level=0.05, strict=TRUE)
    expect_equal(non_strict$power, 0.025)
    expect_equal(strict$power, 0.05)
})

test_that("errors when not exactly one of the mutually exclusive arguments is NULL", {
    expect_error(power_t_test(n=20, delta=1, sd=1, power=0.8, sig.level=0.05))
    expect_error(power_t_test(n=NULL, delta=NULL, sd=1, power=0.8, sig.level=0.05))
})

test_that("errors for nonsensical ratio or sd.ratio", {
    expect_error(power_t_test(n=20, delta=1, sd=1, power=NULL, sig.level=0.05, ratio=0.5),
                 "ratio between group sizes cannot be less than 1")
    expect_error(power_t_test(n=20, delta=1, sd=1, power=NULL, sig.level=0.05, sd.ratio=-1),
                 "sd.ratio between group sd's must be positive")
})
