# Small residual data set with one point outside +/- 1.96 (x = 4)
res_x <- 1:10
res_y <- c(0.1, -0.2, 0.3, 2.5, -0.1, 0.2, -0.3, 0.1, 0, -0.2)

geom_classes <- function(p) {
  unname(vapply(p$layers, function(l) class(l$geom)[1], character(1)))
}

## Base-graphics residualplot() --------------------------------------------

test_that("residualplot draws for numeric vectors, lm and glm fits and returns invisible NULL", {
  withr::local_pdf(NULL)

  expect_invisible(res <- residualplot(res_x, res_y))
  expect_null(res)
  expect_null(residualplot(res_x, res_y, candy = FALSE))
  expect_null(residualplot(res_x, res_y, col.alpha = 0, ylim = c(-5, 5)))
  # three or fewer unique x values: no smoothing spline is added
  expect_null(residualplot(rep(1:3, 4), rep(c(-1, 0, 1), 4)))

  expect_null(residualplot(lm(Volume ~ Girth + Height, data = trees)))
  expect_null(residualplot(glm(am ~ hp, data = mtcars, family = binomial)))
})

test_that("residualplot requires y for the default method", {
  expect_error(residualplot(1:5), "y must be specified")
})

## ggplot2 residual_plot() --------------------------------------------------

test_that("residual_plot with candy = FALSE only draws the points", {
  p <- residual_plot(res_x, res_y, candy = FALSE)
  expect_s3_class(p, "ggplot")
  expect_equal(geom_classes(p), "GeomPoint")
})

test_that("residual_plot with candy adds spline, ribbon and outlier layers", {
  p <- residual_plot(res_x, res_y)
  expect_equal(geom_classes(p), c("GeomPoint", "GeomLine", "GeomRibbon", "GeomPoint", "GeomPoint"))

  # Without points outside +/- 1.96 the outlier layer is omitted
  p_no_outlier <- residual_plot(res_x, c(0.1, -0.2, 0.3, 0.5, -0.1, 0.2, -0.3, 0.1, 0, -0.2))
  expect_equal(geom_classes(p_no_outlier), c("GeomPoint", "GeomLine", "GeomRibbon", "GeomPoint"))
})

test_that("residual_plot ribbon is +/- 1.96 local SD within the bandwidth window", {
  p <- residual_plot(res_x, res_y)
  ribbon <- ggplot2::layer_data(p, 3)

  # window = 0.3 * (10 - 1) / 2 = 1.35, so at x = 1 the window holds x = 1, 2
  expect_equal(ribbon$ymax[1], 1.96 * sd(res_y[1:2]))
  # at x = 5 the window holds x = 4, 5, 6
  expect_equal(ribbon$ymax[5], 1.96 * sd(res_y[4:6]))
  expect_equal(ribbon$ymin, -ribbon$ymax)
})

test_that("residual_plot uses studentized/standardized residuals for lm and glm fits", {
  lm_fit <- lm(Volume ~ Girth + Height, data = trees)
  p_lm <- residual_plot(lm_fit, candy = FALSE)
  expect_equal(p_lm$data$x, unname(predict(lm_fit)))
  expect_equal(p_lm$data$y, unname(rstudent(lm_fit)))

  glm_fit <- glm(am ~ hp, data = mtcars, family = binomial)
  p_glm <- residual_plot(glm_fit, candy = FALSE)
  expect_equal(p_glm$data$x, unname(predict(glm_fit, type = "link")))
  expect_equal(p_glm$data$y, unname(rstandard(glm_fit)))
})

test_that("residual_plot requires y for the default method", {
  expect_error(residual_plot(1:5), "y must be specified")
})
