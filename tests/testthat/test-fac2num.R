test_that("fac2num() returns the numeric value of the levels, not the codes", {
  f <- factor(c(10, 2, 0.5, 2))
  # as.numeric(f) would return the internal codes c(3, 2, 1, 2)
  expect_equal(fac2num(f), c(10, 2, 0.5, 2))
})

test_that("fac2num() returns NA with a warning for non-numeric levels", {
  f <- factor(c("a", "1", "2"))
  expect_warning(res <- fac2num(f), class = "warning")
  expect_equal(res, c(NA, 1, 2))
})

test_that("fac2num() errors for non-factor input", {
  expect_error(fac2num(c(1, 2, 3)), "input needs to be a factor")
  expect_error(fac2num("abc"), "input needs to be a factor")
})
