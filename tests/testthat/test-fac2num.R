context("Test fac2num")

test_that("Converts factor levels to numeric correctly", {
    f <- factor(c(1, 2, 1, 3, 2, 1, 2, 3, 1))
    expect_equal(fac2num(f), c(1, 2, 1, 3, 2, 1, 2, 3, 1))
})

test_that("Non-numeric factor levels produce NA with a warning", {
    f <- factor(c("a", "1", "2"))
    expect_warning(res <- fac2num(f), "NAs introduced by coercion")
    expect_equal(res, c(NA, 1, 2))
})

test_that("Non-factor input raises an error", {
    expect_error(fac2num(c(1, 2, 3)), "input needs to be a factor")
    expect_error(fac2num("abc"), "input needs to be a factor")
})
