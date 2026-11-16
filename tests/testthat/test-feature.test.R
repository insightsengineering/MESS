# A numeric `s` fixes lambda, which skips the (random) cross-validation.
make_feature_data <- function() {
  withr::local_seed(42)
  x <- matrix(rnorm(20 * 5), nrow = 20)
  y <- rnorm(20, mean = 2 * x[, 1])
  list(x = x, y = y)
}

test_that("feature.test returns NA p-values when lambda selects no variables", {
  d <- make_feature_data()
  withr::local_options(warn = 0)

  res <- feature.test(d$x, d$y, B = 5, s = 10, control = list(maxcores = 1))

  expect_named(res, c("p.full", "ols.selected", "p.maxols", "lasso.selected",
                      "p.maxlasso", "lambda.orig", "B"))
  expect_identical(res$p.full, NA)
  expect_identical(res$p.maxols, NA)
  expect_identical(res$p.maxlasso, NA)
  expect_identical(res$lambda.orig, 10)
  expect_identical(res$B, 5)
  expect_equal(getOption("warn"), 0)
})

test_that("feature.test selects the true predictor and returns permutation p-values", {
  skip_known_bug("B13")
  d <- make_feature_data()
  withr::local_seed(1)

  capture.output(res <- feature.test(d$x, d$y, B = 5, s = 0.2, control = list(maxcores = 1)))

  expect_identical(res$lasso.selected[1], 1L)
  expect_identical(res$ols.selected[1], 1L)
  expect_true(all(c(res$p.full, res$p.maxols, res$p.maxlasso) >= 0))
  expect_true(all(c(res$p.full, res$p.maxols, res$p.maxlasso) <= 1))
})
