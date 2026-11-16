context("Test prepost.test")

expo <- c(rep(1, 9), rep(0, 7))
bp1w <- c(137, 120, 141, 137, 140, 144, 134, 123, 142, 139, 134, 136, 151, 147, 137, 149)
bp_base <- c(147, 129, 158, 164, 134, 155, 151, 141, 153, 133, 129, 152, 161, 154, 141, 156)

test_that("documented example returns a valid htest object", {
    res <- prepost.test(bp_base, bp1w, expo)

    expect_s3_class(res, "htest")
    expect_true(all(c("statistic", "se", "p.value", "conf.int", "estimate", "method") %in% names(res)))
    expect_equal(unname(res$estimate), -7.176388, tolerance = 1e-5)
    expect_true(res$p.value >= 0 && res$p.value <= 1)
})

test_that("conf.int width grows with conf.level", {
    res90 <- prepost.test(bp_base, bp1w, expo, conf.level = 0.90)
    res95 <- prepost.test(bp_base, bp1w, expo, conf.level = 0.95)
    res99 <- prepost.test(bp_base, bp1w, expo, conf.level = 0.99)

    width90 <- diff(res90$conf.int)
    width95 <- diff(res95$conf.int)
    width99 <- diff(res99$conf.int)

    expect_true(width90 < width95)
    expect_true(width95 < width99)
    expect_equal(attr(res95$conf.int, "conf.level"), 0.95)
})

test_that("a fixed numeric delta changes the estimated standard error", {
    res_default <- prepost.test(bp_base, bp1w, expo)
    res_delta <- prepost.test(bp_base, bp1w, expo, delta = 0.3)

    expect_false(isTRUE(all.equal(res_default$se, res_delta$se)))
    expect_false(isTRUE(all.equal(res_default$statistic, res_delta$statistic)))
})

test_that("a logical treatment vector works", {
    res <- prepost.test(bp_base, bp1w, as.logical(expo))
    expect_s3_class(res, "htest")
    expect_true(is.finite(res$estimate))
})

test_that("missing post values are handled via the missing-data adjustment", {
    bp1w_na <- bp1w
    bp1w_na[c(2, 10)] <- NA

    res <- suppressWarnings(prepost.test(bp_base, bp1w_na, expo))

    expect_s3_class(res, "htest")
    expect_true(is.finite(res$estimate))
    expect_true(is.finite(res$se))
    expect_true(res$p.value >= 0 && res$p.value <= 1)
})

test_that("more than two distinct treatment values raises an error", {
    treatment3 <- c(rep(1, 6), rep(2, 5), rep(3, 5))
    expect_error(prepost.test(bp_base, bp1w, treatment3), "exactly two treatments")
})

test_that("a disallowed treatment class (character or factor) raises an error", {
    treat_char <- ifelse(expo == 1, "T", "C")
    expect_error(prepost.test(bp_base, bp1w, treat_char), "treatment must be numeric")

    treat_factor <- factor(expo)
    expect_error(prepost.test(bp_base, bp1w, treat_factor), "treatment must be numeric")
})
