test_that("documented example gives the expected rounded percentages", {
  f <- c(1, 2, 1, 3, 2, 1, 2, 3, 1)
  # exact percentages are 6.25, 12.5, 18.75; the missing 1 point after
  # flooring goes to the largest remainders (ties broken by position)
  expect_equal(round_percent(f, ties = "last"), c(6, 12, 6, 19, 13, 6, 13, 19, 6))
})

test_that("random tie-breaking always sums to 100", {
  withr::local_seed(1)
  res <- round_percent(c(1, 1, 1))
  expect_equal(sort(res), c(33, 33, 34))
  expect_equal(sum(res), 100)
})

test_that("decimals sets the precision and the result still sums to 100", {
  # 33.333... each: floor gives 33.3 x 3 = 99.9, the last element gets the extra 0.1
  expect_equal(round_percent(c(1, 1, 1), decimals = 1, ties = "last"), c(33.3, 33.3, 33.4))
  expect_equal(round_percent(c(10, 20, 30, 40), decimals = 1), c(10, 20, 30, 40))
})

test_that("invalid inputs raise informative errors", {
  expect_error(round_percent(c(-1, 2)), "only works on non-negative vectors")
  expect_error(round_percent(c("a", "b")), "only works on numeric vectors")
  expect_error(round_percent(c(1, 2), decimals = -1), "non-negative integer")
})
