context("Test auc")


##
## All of these checks should provide the correct results
##

test_that("Produces validated results", {
    expect_equal(auc(1:4, c(0, 1, 1, 5)), 4.5)
    expect_equal(auc(1:4, -c(0, 1, 1, 5)), -4.5)
    expect_equal(auc(1:4, -c(0, 1, 1, 5), absolutearea=TRUE), 4.5)
    expect_equal(auc(0:4, c(0, 0, 1, -1, 0)), 0)
    expect_equal(auc(0:4, c(0, 0, 1, -1, 0), absolutearea=TRUE), 1.5)
    expect_equal(auc(0:4, c(0, 0, 1, -1, 0), absolutearea=TRUE, subdivisions =150), 1.5)
    expect_equal(auc(0:4, c(0, 0, 1, -1, 0), from=0, rule=2, type="spline", yleft=.5), -0.214285, tolerance=0.00001)
})

test_that("type='spline' gives documented/validated results", {
    expect_equal(auc(1:4, c(0, 1, 1, 5), type="spline"), 4.2, tolerance=0.00001)
    expect_equal(auc(0:4, c(0, 0, 1, -1, 0), type="spline"), -0.2142857, tolerance=0.00001)
})

test_that("absolutearea=TRUE and FALSE differ for spline type when curve crosses zero", {
    linear_signed <- auc(0:4, c(0, 0, 1, -1, 0), type="linear", absolutearea=FALSE)
    linear_abs <- auc(0:4, c(0, 0, 1, -1, 0), type="linear", absolutearea=TRUE)
    expect_true(linear_abs >= abs(linear_signed))

    spline_signed <- auc(0:4, c(0, 0, 1, -1, 0), type="spline", absolutearea=FALSE)
    spline_abs <- auc(0:4, c(0, 0, 1, -1, 0), type="spline", absolutearea=TRUE)
    expect_true(spline_abs > spline_signed)
})

test_that("from/to subsets the x range", {
    expect_equal(auc(1:4, c(0, 1, 1, 5), from=2, to=3), 1)
    expect_equal(auc(1:4, c(0, 1, 1, 5), from=1, to=2), 0.5)
    # Subsetting to the full range matches the default call
    expect_equal(auc(1:4, c(0, 1, 1, 5), from=1, to=4), auc(1:4, c(0, 1, 1, 5)))
})

test_that("auc handles unsorted x values", {
    expect_equal(auc(c(3, 1, 2, 4), c(1, 0, 1, 5)), auc(1:4, c(0, 1, 1, 5)))
})

test_that("a single unique x value returns NA", {
    expect_equal(auc(2, 1), NA)
})

test_that("auc errors on invalid input", {
    expect_error(auc(1:4, c(0, 1, 1)))
    expect_error(auc(c(1, 1, 2, 3), c(0, 1, 1, 5)))
    expect_error(auc(1:4, c(0, 1, 1, 5), from=NA))
})

