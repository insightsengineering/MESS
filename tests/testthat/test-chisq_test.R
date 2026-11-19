# 2x2 table with row sums 20, 10 and column sums 16, 14 (N = 30)
chisq_table <- matrix(c(12, 4, 8, 6), 2)

pearson_statistic <- function(x) {
  e <- outer(rowSums(x), colSums(x)) / sum(x)
  sum((x - e)^2 / e)
}

test_that("monte_carlo_chisq_test() returns the Pearson statistic and expected counts", {
  withr::local_seed(1)
  res <- monte_carlo_chisq_test(chisq_table, B = 1000)

  expect_s3_class(res, "htest")
  expect_named(res, c("statistic", "parameter", "p.value", "method", "data.name", "observed", "expected"))
  expect_equal(unname(res$statistic), pearson_statistic(chisq_table))
  expect_equal(res$expected, outer(c(20, 10), c(16, 14)) / 30)
  expect_equal(res$observed, chisq_table)
  expect_equal(res$parameter, c(B = 1000))
  expect_equal(res$data.name, "chisq_table")
  expect_match(res$method, "fixed N \\(total number of observations\\)")
})

test_that("monte_carlo_chisq_test() p-values are reproducible with a fixed seed", {
  withr::local_seed(1)
  p1 <- monte_carlo_chisq_test(chisq_table, B = 1000)$p.value
  withr::local_seed(1)
  p2 <- monte_carlo_chisq_test(chisq_table, B = 1000)$p.value

  expect_identical(p1, p2)
  expect_equal(p1, 0.3486513, tolerance = 1e-6)
})

test_that("monte_carlo_chisq_test() with margin = 'rows' fixes the row margins", {
  withr::local_seed(1)
  res <- monte_carlo_chisq_test(chisq_table, margin = "rows", B = 1000)

  expect_equal(unname(res$statistic), 15 / 14)
  expect_equal(res$p.value, 0.3736264, tolerance = 1e-6)
  expect_match(res$method, "fixed row margins")
})

test_that("monte_carlo_chisq_test() with margin = 'both' matches chisq.test(simulate.p.value = TRUE)", {
  withr::local_seed(3)
  res <- monte_carlo_chisq_test(chisq_table, margin = "both", B = 1000)
  withr::local_seed(3)
  ref <- chisq.test(chisq_table, correct = FALSE, simulate.p.value = TRUE, B = 1000)

  expect_equal(res$p.value, ref$p.value)
  expect_equal(unname(res$statistic), unname(ref$statistic))
  expect_match(res$method, "fixed row AND column margins")
})

test_that("monte_carlo_chisq_test() accepts a data frame", {
  withr::local_seed(1)
  res <- monte_carlo_chisq_test(as.data.frame(chisq_table), B = 100)
  expect_equal(unname(res$statistic), 15 / 14)
})

test_that("monte_carlo_chisq_test() validates its input", {
  expect_error(monte_carlo_chisq_test(matrix(c(-1, 4, 8, 6), 2), B = 10), "nonnegative and finite")
  expect_error(monte_carlo_chisq_test(matrix(c(1, NA, 8, 6), 2), B = 10), "nonnegative and finite")
  expect_error(monte_carlo_chisq_test(matrix(0, 2, 2), B = 10), "at least one entry")
  expect_error(monte_carlo_chisq_test(matrix(1:3, 1), B = 10), "2-way tables")
  expect_error(monte_carlo_chisq_test(1:4, B = 10), "convertible with a matrix")
  expect_error(monte_carlo_chisq_test(chisq_table, margin = "cols"), "should be one of")
})

test_that("monte_carlo_chisq_test() rejects zero margins when they are fixed", {
  zero_row <- matrix(c(0, 4, 0, 6), 2)
  zero_col <- matrix(c(0, 0, 4, 6), 2)

  expect_error(monte_carlo_chisq_test(zero_row, margin = "rows", B = 10), "row marginals")
  expect_error(monte_carlo_chisq_test(zero_row, margin = "both", B = 10), "row marginals")
  expect_error(monte_carlo_chisq_test(zero_col, margin = "both", B = 10), "column marginals")
})
