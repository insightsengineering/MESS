# All tests use hide = FALSE: with hide = TRUE the plot waits for <Enter>
# via readline(), and the original data is always drawn in the first panel.

# Panel function that plots and records the y values it is asked to draw
recording_panel <- function(env) {
  force(env)
  function(x, y, ...) {
    env$calls <- c(env$calls, list(y))
    plot(x, y, ...)
  }
}

test_that("wallyplot draws 9 panels with the original data first and returns invisible NULL", {
  withr::local_pdf(NULL)
  withr::local_seed(1)
  rec <- new.env()
  y <- rnorm(20)

  expect_invisible(res <- wallyplot(1:20, y, FUN = recording_panel(rec), hide = FALSE))
  expect_null(res)
  expect_length(rec$calls, 9)
  expect_equal(rec$calls[[1]], y)
})

test_that("a 9-column matrix of y values is plotted as given", {
  withr::local_pdf(NULL)
  rec <- new.env()
  y <- matrix(1:90, ncol = 9)

  wallyplot(1:10, y, FUN = recording_panel(rec), hide = FALSE)
  expect_equal(rec$calls, lapply(1:9, function(j) y[, j]))
})

test_that("the default residualplot panel function draws without error", {
  withr::local_pdf(NULL)
  withr::local_seed(2)
  expect_null(wallyplot(1:20, rnorm(20), hide = FALSE))
})

test_that("wallyplot for an lm fit shows the standardized residuals first", {
  withr::local_pdf(NULL)
  withr::local_seed(3)
  rec <- new.env()
  fit <- lm(Volume ~ Girth + Height, data = trees)

  expect_null(wallyplot(fit, FUN = recording_panel(rec), hide = FALSE))
  expect_length(rec$calls, 9)
  expect_equal(rec$calls[[1]], rstandard(fit))
})

test_that("invalid inputs raise informative errors", {
  expect_error(wallyplot(1:20, rnorm(15), hide = FALSE), "must be a vector and matrix")
  expect_error(wallyplot(1:20, matrix(0, 20, 5), hide = FALSE), "y must be a matrix with 9 columns")
  expect_error(wallyplot(1:20, matrix(0, 15, 9), hide = FALSE), "x and y does not conform")
})
