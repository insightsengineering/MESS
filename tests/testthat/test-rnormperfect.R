test_that("values are the normal quantiles at i / (n + 1)", {
  expect_equal(rnorm_perfect(4), qnorm((1:4) / 5))
  expect_equal(rnorm_perfect(5, sd = 2), 2 * qnorm((1:5) / 6))
  # middle value of an odd-length sample is exactly the mean
  expect_equal(rnorm_perfect(5)[3], 0)
})

test_that("a vector n uses its length, like rnorm()", {
  expect_equal(rnorm_perfect(c(10, 20, 30)), rnorm_perfect(3))
})

test_that("invalid arguments raise errors", {
  expect_error(rnorm_perfect("a"), "is.numeric\\(n\\) is not TRUE")
  expect_error(rnorm_perfect(0), "n > 0 is not TRUE")
  expect_error(rnorm_perfect(5, mean = c(0, 1)), "length\\(mean\\) == 1 is not TRUE")
  expect_error(rnorm_perfect(5, sd = c(1, 2)), "length\\(sd\\) == 1 is not TRUE")
})
