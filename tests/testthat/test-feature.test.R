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
