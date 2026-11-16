test_that("documented example: row means over the non-missing values", {
  expect_equal(conditional_rowMeans(1:5, c(1:4, NA), c(1:3, NA, NA)), c(1, 2, 3, 4, 5))
})

test_that("rows with fewer than minobs observations are NA", {
  x <- c(1, NA, 3)
  y <- c(3, NA, NA)
  z <- c(5, 6, NA)

  # minobs = 1 (default): only the all-missing row is NA
  expect_equal(conditional_rowMeans(c(1, NA), c(NA, NA)), c(1, NA))
  expect_equal(conditional_rowMeans(x, y, z), c(3, 6, 3))
  expect_equal(conditional_rowMeans(x, y, z, minobs = 2), c(3, NA, NA))
  expect_equal(conditional_rowMeans(x, y, z, minobs = 3), c(3, NA, NA))
  expect_equal(conditional_rowMeans(1:5, c(1:4, NA), c(1:3, NA, NA), minobs = 2), c(1, 2, 3, 4, NA))
})

test_that("a matrix argument is treated as several columns", {
  m <- cbind(c(1, NA), c(3, 4))
  expect_equal(conditional_rowMeans(m), c(2, 4))
  expect_equal(conditional_rowMeans(m, minobs = 2), c(2, NA))
})
