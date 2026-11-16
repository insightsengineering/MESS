context("Test usd")

test_that("Matches documented example value and differs from sd", {
    expect_equal(usd(1:5), 1.681951, tolerance = 1e-6)
    expect_false(isTRUE(all.equal(usd(1:5), sd(1:5))))
})

test_that("na.rm removes missing values before computing", {
    expect_equal(usd(c(1:5, NA), na.rm = TRUE), usd(1:5))
    expect_true(is.na(usd(c(1:5, NA))))
})

test_that("Length-one and zero-length vectors return NA", {
    expect_true(is.na(usd(1)))
    expect_true(is.na(usd(numeric(0))))
})

test_that("Non-numeric input produces a coercion warning", {
    expect_warning(res <- usd(c("a", "b")), "NAs introduced by coercion")
    expect_true(is.na(res))
})
