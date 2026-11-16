context("Test gkgamma")

test_that("documented example produces expected htest object", {
    smoke <- matrix(c(16, 15, 13, 10, 1, 73, 75, 59, 81, 29, 6, 6, 7, 17, 3, 1, 0, 1, 3, 1), ncol = 4)
    colnames(smoke) <- c("VGood", "Good", "Fair", "Bad")
    rownames(smoke) <- c("Never", "No more", "1-14", "15-24", "25+")

    res <- gkgamma(smoke)

    expect_s3_class(res, "htest")
    expect_true(all(c("statistic", "p.value", "estimate", "conf.int", "method",
                       "data.name", "observed", "se0", "se1") %in% names(res)))
    expect_equal(unname(res$estimate), 0.2418174, tolerance = 1e-6)
    expect_true(res$estimate >= -1 && res$estimate <= 1)
})

test_that("estimate is always within [-1, 1]", {
    set.seed(1)
    tab <- matrix(sample(1:20, 9, replace = TRUE), nrow = 3)
    res <- gkgamma(tab)
    expect_true(res$estimate >= -1 && res$estimate <= 1)
})

test_that("a diagonal-dominant table gives an estimate close to 1", {
    diagtab <- matrix(c(100, 1, 1, 1, 100, 1, 1, 1, 100), nrow = 3, byrow = TRUE)
    res <- gkgamma(diagtab)
    expect_true(res$estimate > 0.95)
})

test_that("conf.int width grows monotonically with conf.level", {
    smoke <- matrix(c(16, 15, 13, 10, 1, 73, 75, 59, 81, 29, 6, 6, 7, 17, 3, 1, 0, 1, 3, 1), ncol = 4)

    width90 <- diff(gkgamma(smoke, conf.level = 0.90)$conf.int)
    width95 <- diff(gkgamma(smoke, conf.level = 0.95)$conf.int)
    width99 <- diff(gkgamma(smoke, conf.level = 0.99)$conf.int)

    expect_true(width90 < width95)
    expect_true(width95 < width99)
})

test_that("gkgamma errors on input that cannot be treated as a numeric matrix", {
    bad_df <- data.frame(a = c("x", "y"), b = c("z", "w"))
    expect_error(gkgamma(bad_df))
    expect_error(gkgamma(list(1, 2, 3)))
})
