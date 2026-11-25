test_that("Produces validated results", {
    expect_equal(auc(1:4, c(0, 1, 1, 5)), 4.5)
    expect_equal(auc(1:4, -c(0, 1, 1, 5)), -4.5)
    expect_equal(auc(1:4, -c(0, 1, 1, 5), absolutearea=TRUE), 4.5)
    expect_equal(auc(0:4, c(0, 0, 1, -1, 0)), 0)
    expect_equal(auc(0:4, c(0, 0, 1, -1, 0), absolutearea=TRUE), 1.5)
    expect_equal(auc(0:4, c(0, 0, 1, -1, 0), absolutearea=TRUE, subdivisions =150), 1.5)
    expect_equal(auc(0:4, c(0, 0, 1, -1, 0), from=0, rule=2, type="spline", yleft=.5), -0.214285, tolerance=0.00001)
})

test_that("auc() with type = 'spline' integrates the natural spline", {
  x <- 0:4
  y <- c(0, 0, 1, -1, 0)
  f <- splinefun(x, y, method = "natural")

  expect_equal(
    auc(x, y, type = "spline", absolutearea = TRUE),
    integrate(function(z) abs(f(z)), 0, 4)$value
  )
  expect_equal(auc(x, y, type = "spline", from = 1, to = 3), integrate(f, 1, 3)$value)
})

test_that("auc() returns NA for a single x value", {
  expect_identical(auc(2, 1), NA)
})

test_that("auc() with absolutearea = TRUE handles unsorted x values", {
  expect_equal(
    auc(c(4, 0, 1, 2, 3), c(0, 0, 0, 1, -1), absolutearea = TRUE),
    auc(c(0, 1, 2, 3), c(0, 1, -1, 0), absolutearea = TRUE)
  )
})
