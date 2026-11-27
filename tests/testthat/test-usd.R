# usd(x) = sd(x) / (1 - 1/(4n) - 7/(32n^2) - 19/(128n^3))

test_that("usd applies the small-sample correction to sd", {
  n <- 5
  correction <- 1 - 1 / (4 * n) - 7 / (32 * n^2) - 19 / (128 * n^3)
  expect_equal(usd(1:5), sd(1:5) / correction)
  # documented value
  expect_equal(usd(1:5), 1.681951, tolerance = 1e-6)
})

test_that("na.rm drops missing values and uses the reduced n", {
  expect_equal(usd(c(1:5, NA), na.rm = TRUE), usd(1:5))
  expect_equal(usd(c(1:5, NA)), NA_real_)
})

test_that("fewer than two observations give NA", {
  expect_equal(usd(1), NA_real_)
  expect_equal(usd(numeric(0)), NA_real_)
})
