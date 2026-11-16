test_that("auc() gives the trapezoid area for type = 'linear'", {
  # Trapezoids: (0+1)/2 + (1+1)/2 + (1+5)/2 = 4.5
  expect_equal(auc(1:4, c(0, 1, 1, 5)), 4.5)
  expect_equal(auc(1:4, -c(0, 1, 1, 5)), -4.5)
  # Positive and negative parts cancel
  expect_equal(auc(0:4, c(0, 0, 1, -1, 0)), 0)
})

test_that("auc() with absolutearea = TRUE integrates |y|", {
  expect_equal(auc(1:4, -c(0, 1, 1, 5), absolutearea = TRUE), 4.5)
  # Two triangles above and two below the axis, each of area 0.25 or 0.5
  expect_equal(auc(0:4, c(0, 0, 1, -1, 0), absolutearea = TRUE), 1.5)
})

test_that("auc() restricts the range with from/to", {
  expect_equal(auc(1:4, c(0, 1, 1, 5), from = 2, to = 3), 1)
  # Interpolated end points: y(1.5) = 0.5, y(3.5) = 3
  expect_equal(auc(1:4, c(0, 1, 1, 5), from = 1.5, to = 3.5), 2.375)
})

test_that("auc() passes ... to approx() for extrapolation", {
  # rule = 2 extends the end values: 0 on [0, 1] and 5 on [4, 5]
  expect_equal(auc(1:4, c(0, 1, 1, 5), from = 0, to = 5, rule = 2), 9.5)
  # Without rule = 2 the outside values are NA
  expect_identical(auc(1:4, c(0, 1, 1, 5), from = 0, to = 5), NA_real_)
})

test_that("auc() sorts unsorted x values", {
  expect_equal(auc(c(3, 1, 2, 4), c(1, 0, 1, 5)), 4.5)
})

test_that("auc() with absolutearea = TRUE handles unsorted x values", {
  skip_known_bug("B01")
  expect_equal(auc(c(4, 0, 1, 2, 3), c(0, 0, 0, 1, -1), absolutearea = TRUE), 1.5)
})

test_that("auc() with type = 'spline' integrates the natural spline", {
  x <- 0:4
  y <- c(0, 0, 1, -1, 0)
  f <- splinefun(x, y, method = "natural")

  expect_equal(auc(1:4, c(0, 1, 1, 5), type = "spline"), 4.2, tolerance = 1e-6)
  expect_equal(auc(x, y, type = "spline"), integrate(f, 0, 4)$value)
  expect_equal(auc(x, y, type = "spline"), -0.2142857, tolerance = 1e-6)
  expect_equal(
    auc(x, y, type = "spline", absolutearea = TRUE),
    integrate(function(z) abs(f(z)), 0, 4)$value
  )
  expect_equal(auc(x, y, type = "spline", from = 1, to = 3), integrate(f, 1, 3)$value)
})

test_that("auc() returns NA for a single x value", {
  expect_identical(auc(2, 1), NA)
})

test_that("auc() validates its input", {
  expect_error(auc(1:4, c(0, 1, 1)), "length\\(x\\) == length\\(y\\)")
  expect_error(auc(c(1, 1, 2, 3), c(0, 1, 1, 5)), "length\\(unique\\(x\\)\\)")
  expect_error(auc(1:4, c(0, 1, 1, 5), from = NA), "is.na\\(from\\)")
  expect_error(auc(1:4, c(0, 1, 1, 5), type = "cubic"), "should be one of")
})
