test_that("orcor() default type gives known odds ratios", {
  # OR = (1 + rho) / (1 - rho)
  expect_equal(orcor(0), 1)
  expect_equal(orcor(0.5), 3)
  expect_equal(orcor(-0.5), 1 / 3)
  expect_equal(orcor(1), Inf)
  expect_equal(orcor(-1), 0)
})

test_that("orcor() is vectorised over rho", {
  expect_equal(orcor(c(0, 0.5, -0.5)), c(1, 3, 1 / 3))
})

test_that("orcor() youden type gives known odds ratios", {
  # l = 2 * asin(rho) / pi; rho = sin(pi / 4) gives l = 0.5, so OR = (1.5 / 0.5)^2 = 9
  expect_equal(orcor(0, type = "youden"), 1)
  expect_equal(orcor(sin(pi / 4), type = "youden"), 9)
  expect_equal(orcor(-sin(pi / 4), type = "youden"), 1 / 9)
  expect_equal(orcor(1, type = "youden"), Inf)
})

test_that("orcor() youden type ignores lambda", {
  expect_equal(orcor(0.3, type = "youden", lambda = 3), orcor(0.3, type = "youden", lambda = 0))
})
