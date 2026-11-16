test_that("panel.hist and panel.r2 work together through pairs()", {
  pdf(tempfile())
  on.exit(dev.off())
  expect_no_error(
    pairs(~ Ozone + Temp + Wind + Solar.R, data = airquality,
          lower.panel = panel.smooth, diag.panel = panel.hist,
          upper.panel = panel.r2)
  )
})

test_that("panel.hist draws a histogram without error for a numeric vector", {
  pdf(tempfile())
  on.exit(dev.off())
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1))
  set.seed(1)
  expect_no_error(panel.hist(rnorm(50)))
})

test_that("panel.hist accepts a custom col.bar argument", {
  pdf(tempfile())
  on.exit(dev.off())
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1))
  set.seed(1)
  expect_no_error(panel.hist(rnorm(50), col.bar = "red"))
})

test_that("panel.r2 draws R2 text without error for a numeric pair", {
  pdf(tempfile())
  on.exit(dev.off())
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1))
  set.seed(2)
  expect_no_error(panel.r2(rnorm(30), rnorm(30)))
})

test_that("panel.r2 accepts a custom digits argument", {
  pdf(tempfile())
  on.exit(dev.off())
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1))
  set.seed(3)
  expect_no_error(panel.r2(rnorm(30), rnorm(30), digits = 4))
})

test_that("panel.r2 accepts a custom cex.cor argument", {
  pdf(tempfile())
  on.exit(dev.off())
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1))
  set.seed(4)
  expect_no_error(panel.r2(rnorm(30), rnorm(30), cex.cor = 2))
})

test_that("panel.r2 handles both highly correlated and uncorrelated data", {
  pdf(tempfile())
  on.exit(dev.off())
  set.seed(5)
  x <- rnorm(30)
  y_correlated <- x * 2 + rnorm(30, sd = 0.01)
  y_uncorrelated <- rnorm(30)

  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1))
  expect_no_error(panel.r2(x, y_correlated))

  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1))
  expect_no_error(panel.r2(x, y_uncorrelated))
})
