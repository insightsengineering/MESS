test_that("gkgamma equals Yule's Q for a 2x2 table", {
  tab <- matrix(c(10, 5, 3, 12), nrow = 2)
  # Yule's Q = (ad - bc) / (ad + bc) = (120 - 15) / (120 + 15)
  expect_equal(unname(gkgamma(tab)$estimate), 105 / 135)
})

test_that("gkgamma gives +1 / -1 for perfectly concordant / discordant tables", {
  expect_equal(unname(gkgamma(diag(c(5, 5, 5)))$estimate), 1)
  expect_equal(unname(gkgamma(diag(c(5, 5, 5))[, 3:1])$estimate), -1)
})

test_that("gkgamma reproduces the documented smoking example", {
  smoke <- matrix(
    c(16, 15, 13, 10, 1, 73, 75, 59, 81, 29, 6, 6, 7, 17, 3, 1, 0, 1, 3, 1),
    ncol = 4
  )
  res <- gkgamma(smoke)

  expect_s3_class(res, "htest")
  expect_equal(unname(res$estimate), 0.2418173666, tolerance = 1e-8)
  expect_equal(res$se0, 0.07334398549, tolerance = 1e-8)
  expect_equal(res$se1, 0.07066251422, tolerance = 1e-8)
  expect_equal(unname(res$statistic), 3.297030629, tolerance = 1e-8)
  expect_equal(res$data.name, "smoke")
  expect_identical(res$observed, smoke)
})

test_that("gkgamma p-value and confidence interval follow from estimate and SEs", {
  smoke <- matrix(
    c(16, 15, 13, 10, 1, 73, 75, 59, 81, 29, 6, 6, 7, 17, 3, 1, 0, 1, 3, 1),
    ncol = 4
  )
  res <- gkgamma(smoke, conf.level = 0.90)
  est <- unname(res$estimate)

  expect_equal(unname(res$statistic), est / res$se0)
  expect_equal(res$p.value, 2 * pnorm(-abs(est / res$se0)))
  expect_equal(
    as.vector(res$conf.int),
    est + c(-1, 1) * qnorm(0.95) * res$se1
  )
  expect_identical(attr(res$conf.int, "conf.level"), 0.90)
})
