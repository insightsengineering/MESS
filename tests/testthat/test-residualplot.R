## -------------------------------------------------------------------------
## Base-graphics family: residualplot / residualplot.default / .lm / .glm
## -------------------------------------------------------------------------

test_that("residualplot works for an lm object and returns invisible NULL", {
  pdf(tempfile())
  on.exit(dev.off())
  model <- lm(Volume ~ Girth + Height, data = trees)
  result <- residualplot(model)
  expect_null(result)
})

test_that("residualplot works for a glm object", {
  pdf(tempfile())
  on.exit(dev.off())
  gmodel <- glm(am ~ hp, data = mtcars, family = binomial)
  expect_no_error(residualplot(gmodel))
})

test_that("residualplot.default works for plain numeric vectors", {
  pdf(tempfile())
  on.exit(dev.off())
  set.seed(1)
  expect_no_error(residualplot(1:20, rnorm(20)))
})

test_that("residualplot works with candy=FALSE", {
  pdf(tempfile())
  on.exit(dev.off())
  model <- lm(Volume ~ Girth + Height, data = trees)
  expect_no_error(residualplot(model, candy = FALSE))
})

test_that("residualplot accepts a custom ylim", {
  pdf(tempfile())
  on.exit(dev.off())
  set.seed(1)
  expect_no_error(residualplot(1:20, rnorm(20), ylim = c(-5, 5)))
})

test_that("residualplot errors when y is not specified", {
  pdf(tempfile())
  on.exit(dev.off())
  expect_error(residualplot(1:5), "y must be specified")
})

## -------------------------------------------------------------------------
## ggplot2-based family: residual_plot / residual_plot.default / .lm / .glm
## -------------------------------------------------------------------------

test_that("residual_plot returns a ggplot object for an lm model", {
  model <- lm(Volume ~ Girth + Height, data = trees)
  p <- residual_plot(model)
  expect_s3_class(p, "ggplot")
})

test_that("residual_plot returns a ggplot object for a glm model", {
  gmodel <- glm(am ~ hp, data = mtcars, family = binomial)
  p <- residual_plot(gmodel)
  expect_s3_class(p, "ggplot")
})

test_that("residual_plot returns a ggplot object for plain numeric vectors", {
  set.seed(1)
  p <- residual_plot(1:20, rnorm(20))
  expect_s3_class(p, "ggplot")
})

test_that("residual_plot returns a valid ggplot with candy=TRUE and candy=FALSE", {
  model <- lm(Volume ~ Girth + Height, data = trees)
  p_candy <- residual_plot(model, candy = TRUE)
  p_nocandy <- residual_plot(model, candy = FALSE)
  expect_s3_class(p_candy, "ggplot")
  expect_s3_class(p_nocandy, "ggplot")
})

test_that("residual_plot.default errors when y is not specified", {
  expect_error(residual_plot(1:5), "y must be specified")
})
