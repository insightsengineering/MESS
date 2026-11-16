context("Test pairwise.cor.test")

test_that("documented example returns a pairwise.htest object", {
    Month <- factor(airquality$Month, labels = month.abb[5:9])
    res <- suppressWarnings(with(airquality, pairwise.cor.test(Ozone, Month)))

    expect_s3_class(res, "pairwise.htest")
    expect_true(all(c("method", "data.name", "p.value", "p.adjust.method") %in% names(res)))
    expect_equal(res$method, "Pearson's product-moment correlation")
})

test_that("p.adjust.method is recorded and affects the adjusted p-values", {
    Month <- factor(airquality$Month, labels = month.abb[5:9])
    res_default <- suppressWarnings(with(airquality, pairwise.cor.test(Ozone, Month)))
    res_bonf <- suppressWarnings(with(airquality, pairwise.cor.test(Ozone, Month, p.adjust.method = "bonf")))

    expect_equal(res_default$p.adjust.method, "holm")
    expect_equal(res_bonf$p.adjust.method, "bonferroni")
    expect_false(isTRUE(all.equal(res_default$p.value, res_bonf$p.value)))
})

test_that("method argument accepts pearson, kendall and spearman", {
    Month <- factor(airquality$Month, labels = month.abb[5:9])

    res_p <- suppressWarnings(with(airquality, pairwise.cor.test(Ozone, Month, method = "pearson")))
    res_k <- suppressWarnings(with(airquality, pairwise.cor.test(Ozone, Month, method = "kendall")))
    res_s <- suppressWarnings(with(airquality, pairwise.cor.test(Ozone, Month, method = "spearman")))

    expect_equal(res_p$method, "Pearson's product-moment correlation")
    expect_equal(res_k$method, "Kendall's rank correlation tau")
    expect_equal(res_s$method, "Spearman's rank correlation rho")
})

test_that("mismatched group sizes yield NA p-values (with a warning) instead of an error", {
    set.seed(42)
    x <- c(rnorm(5), rnorm(7), rnorm(5))
    g <- factor(c(rep("A", 5), rep("B", 7), rep("C", 5)))

    expect_warning(res <- pairwise.cor.test(x, g), "Group sizes are not identical")

    expect_s3_class(res, "pairwise.htest")
    # A vs B and B vs C have mismatched sizes (5 vs 7) -> NA
    expect_true(is.na(res$p.value["B", "A"]))
    expect_true(is.na(res$p.value["C", "B"]))
    # A vs C have matching sizes (5 vs 5) -> a real p-value
    expect_false(is.na(res$p.value["C", "A"]))
    expect_true(res$p.value["C", "A"] >= 0 && res$p.value["C", "A"] <= 1)
})

test_that("invalid method and p.adjust.method arguments raise an error", {
    Month <- factor(airquality$Month, labels = month.abb[5:9])
    expect_error(with(airquality, pairwise.cor.test(Ozone, Month, method = "notamethod")))
    expect_error(with(airquality, pairwise.cor.test(Ozone, Month, p.adjust.method = "notamethod")))
})
