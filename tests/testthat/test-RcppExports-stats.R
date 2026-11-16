# Statistical helpers implemented in C++ (src/), checked against hand-computed
# values or base R equivalents.

# Reference implementation of the (V-statistic) distance covariance
dcov_r <- function(x, y) {
  centre <- function(d) d - outer(rowMeans(d), colMeans(d), "+") + mean(d)
  a <- centre(as.matrix(dist(x)))
  b <- centre(as.matrix(dist(y)))
  sqrt(mean(a * b))
}

test_that("bin() counts values in equal-width bins", {
  # bins [0,3), [3,6), [6,9), [9,12)
  res <- bin(c(1, 2, 5, 6, 9), 3)
  expect_named(res, c("counts", "origin", "width", "missing", "last_bin_is_missing"))
  expect_equal(res$counts, c(2, 1, 1, 1))
  expect_equal(res$origin, 0)
  expect_equal(res$width, 3)
  expect_equal(res$missing, 0)
  expect_false(res$last_bin_is_missing)
})

test_that("bin() drops values below origin", {
  # bins [2,5), [5,8), [8,11); the value 1 is dropped
  res <- bin(c(1, 2, 5, 6, 9), 3, origin = 2)
  expect_equal(res$counts, c(1, 2, 1))
  expect_equal(res$origin, 2)
})

test_that("bin() counts missing values and optionally appends them", {
  x <- c(1, 2, NA, 3, NaN, 4)

  res <- bin(x, 2, missinglast = TRUE)
  expect_equal(res$counts, c(1, 2, 1, 2))
  expect_equal(res$missing, 2)
  expect_true(res$last_bin_is_missing)

  res <- bin(x, 2)
  expect_equal(res$counts, c(1, 2, 1))
  expect_equal(res$missing, 2)
})

test_that("bin() errors for a non-positive width", {
  expect_error(bin(1:5, 0), "width must be positive")
  expect_error(bin(1:5, -1), "width must be positive")
})

test_that("cumsumbinning() starts a new group before the threshold is exceeded", {
  x <- c(3, 4, 5, 12, 1, 5, 3)
  expect_equal(cumsumbinning(x, 10), c(1, 1, 2, 3, 4, 4, 4))
  expect_equal(cumsumbinning(x, 10, cutwhenpassed = TRUE), c(1, 1, 1, 2, 3, 3, 3))
})

test_that("cumsumbinning() caps the group size with maxgroupsize", {
  x <- c(3, 4, 5, 12, 1, 5, 3)
  expect_equal(cumsumbinning(x, 10, maxgroupsize = 2), c(1, 1, 2, 3, 4, 4, 5))
  expect_equal(cumsumbinning(x, 10, cutwhenpassed = TRUE, maxgroupsize = 2), c(1, 1, 2, 2, 3, 3, 4))
  expect_error(cumsumbinning(x, 10, maxgroupsize = 0), "maxgroupsize should be larger than 0")
})

test_that("cumsumbinning() numbers groups from 1 when the first value exceeds the threshold", {
  skip_known_bug("B24")
  expect_equal(cumsumbinning(c(15, 1, 2), 10), c(1, 2, 2))
})

test_that("dCov() and dCor() match the distance covariance definition", {
  x <- matrix(c(1, 2, 4))
  y <- matrix(c(1, 3, 2))

  expect_equal(dCov(x, y), dcov_r(x, y))
  expect_equal(dCov(x, y), 2 / 3) # hand-computed from the centred distance matrices
  expect_equal(dCor(x, y), dcov_r(x, y) / sqrt(dcov_r(x, x) * dcov_r(y, y)))
})

test_that("dCov() and dCor() handle multi-column matrices", {
  withr::local_seed(7)
  x <- matrix(rnorm(20), ncol = 2)
  y <- matrix(rnorm(20), ncol = 2)
  expect_equal(dCov(x, y), dcov_r(x, y))
  expect_equal(dCor(x, y), dcov_r(x, y) / sqrt(dcov_r(x, x) * dcov_r(y, y)))
})

test_that("dCor() is 1 for a linear relationship", {
  x <- matrix(1:10)
  expect_equal(dCor(x, 2 * x), 1)
  expect_equal(dCor(x, x), 1)
})

test_that("dCov() and dCor() error for different numbers of rows", {
  expect_error(dCov(matrix(1:10), matrix(1:8)), "same number of rows")
  expect_error(dCor(matrix(1:10), matrix(1:8)), "same number of rows")
})

test_that("pairwise_distance_correlation() returns dCor^2 per column", {
  withr::local_seed(10)
  y <- rnorm(30)
  x <- matrix(rnorm(30 * 4), ncol = 4)
  expected <- vapply(1:4, function(k) dCor(x[, k, drop = FALSE], matrix(y))^2, numeric(1))

  expect_equal(as.numeric(pairwise_distance_correlation(x, y)), expected)
})

test_that("pairwise_distance_correlation() errors on mismatched rows", {
  # the message comes from Armadillo, so only the error is checked
  expect_error(pairwise_distance_correlation(matrix(1:10, ncol = 2), 1:3))
})

test_that("filldown() carries the last non-missing value forward", {
  expect_equal(
    filldown(c(1:5, "Howdy", NA, NA, 2:3, NA)),
    c("1", "2", "3", "4", "5", "Howdy", "Howdy", "Howdy", "2", "3", "3")
  )
  expect_equal(filldown(c(NA, NA, NA, 3:5)), c(NA, NA, NA, 3, 4, 5))
  expect_equal(filldown(c(NA, 1, NA, 2)), c(NA, 1, 1, 2))
  expect_equal(filldown(c(1, 2, 3)), c(1, 2, 3))
  expect_equal(filldown(c(TRUE, NA, FALSE, NA)), c(TRUE, TRUE, FALSE, FALSE))
})

test_that("hwe_frequencies() computes allele and genotype frequencies", {
  # Genotypes 1/1, 1/2, 2/2, 2/1, 1/1, 2/2: p = q = 0.5
  res <- hwe_frequencies(c(1, 1, 2, 2, 1, 2), c(1, 2, 2, 1, 1, 2))

  expect_named(res, c("allele_freq", "genotype_freq", "obs_genotype", "available_genotypes", "unique_alleles"))
  expect_equal(res$allele_freq, c(0.5, 0.5))
  expect_equal(res$genotype_freq, c(0.25, 0.5, 0.25)) # p^2, 2pq, q^2
  expect_equal(res$obs_genotype, c(2L, 2L, 2L))
  expect_equal(res$available_genotypes, 6L)
  expect_equal(res$unique_alleles, 2L)
})

test_that("hwe_frequencies() pads allele frequencies to min_alleles", {
  res <- hwe_frequencies(c(1, 1, 2, 2, 1, 2), c(1, 2, 2, 1, 1, 2), min_alleles = 4)
  expect_equal(res$allele_freq, c(0.5, 0.5, 0, 0))
})

test_that("hwe_frequencies() errors for alleles of different lengths", {
  expect_error(hwe_frequencies(c(1, 2, 3), c(1, 2)), "lengths of allele1 and allele2 must match")
})

test_that("ks_cumtest() computes the maximal cumulative difference", {
  withr::local_seed(1)
  res <- ks_cumtest(1:6, B = 1000)

  # max |cumsum(x) / 21 - (1:6) / 6| is reached at k = 3: |6/21 - 1/2| = 3/14
  expect_s3_class(res, "htest")
  expect_equal(unname(res$statistic), 3 / 14)
  expect_equal(res$p.value, 0.1038961, tolerance = 1e-6)
})

test_that("ks_cumtest() uses prob as the null distribution", {
  withr::local_seed(1)
  res <- ks_cumtest(1:6, B = 100, prob = c(5, 1, 1, 1, 1, 1) / 10)
  # max |cumsum(1:6) / 21 - cumsum(prob)| is reached at k = 2: |3/21 - 6/10|
  expect_equal(unname(res$statistic), 6 / 10 - 3 / 21)
})

test_that("ks_cumtest() rescales prob to sum to 1", {
  skip_known_bug("B22")
  withr::local_seed(1)
  res <- ks_cumtest(1:6, B = 100, prob = c(5, 1, 1, 1, 1, 1))
  expect_equal(unname(res$statistic), 6 / 10 - 3 / 21)
})

test_that("ks_cumtest() validates its input", {
  expect_error(ks_cumtest(1:6, B = 100, prob = c(1, 1, 1)), "lengths of x and prob must match")
  expect_error(ks_cumtest(1:6, B = 100, prob = c(1, 1, 1, 1, 1, 0)), "must be positive")
  expect_error(ks_cumtest(1:6, B = 0), "must be greater than 0")
})

test_that("maximum_subarray() finds the contiguous subarray with the largest sum", {
  expect_equal(maximum_subarray(1:4), list(sum = 10, start = 1, end = 4))
  expect_equal(maximum_subarray(c(-2, 1, -3, 4, -1, 2, 1, -5, 4)), list(sum = 6, start = 4, end = 7))
  expect_equal(maximum_subarray(c(-5, -2, -8, -1, -9)), list(sum = -1, start = 4, end = 4))
  expect_equal(maximum_subarray(7), list(sum = 7, start = 1, end = 1))
})

test_that("maximum_subarray() rejects an empty vector", {
  skip_known_bug("B23")
  expect_error(maximum_subarray(numeric(0)))
})

test_that("mfastLmCpp() fits one simple linear regression per column", {
  X <- cbind(1:6, c(2, 1, 4, 3, 6, 5))
  y <- c(1, 3, 2, 5, 4, 6)
  slope <- function(fit) unname(summary(fit)$coefficients[2, 1:3])

  res <- mfastLmCpp(y, X)
  expect_named(res, c("coefficients", "stderr", "tstat"))
  expect_equal(unlist(res[1, ]), slope(lm(y ~ X[, 1])), ignore_attr = TRUE)
  expect_equal(unlist(res[2, ]), slope(lm(y ~ X[, 2])), ignore_attr = TRUE)

  res <- mfastLmCpp(y, X, addintercept = FALSE)
  expect_equal(
    unlist(res[1, ]),
    unname(summary(lm(y ~ X[, 1] - 1))$coefficients[1, 1:3]),
    ignore_attr = TRUE
  )
})

test_that("pairwise_Schur_product() multiplies all pairs of columns", {
  X <- cbind(rep(1, 4), 1:4, 4:1)

  expect_equal(
    unname(pairwise_Schur_product(X)),
    cbind(X[, 1] * X[, 2], X[, 1] * X[, 3], X[, 2] * X[, 3])
  )
  expect_equal(
    unname(pairwise_Schur_product(X, self = TRUE)),
    cbind(X[, 1]^2, X[, 1] * X[, 2], X[, 1] * X[, 3], X[, 2]^2, X[, 2] * X[, 3], X[, 3]^2)
  )
  expect_equal(dim(pairwise_Schur_product(matrix(1:4))), c(4, 0))
})

test_that("pairwise_combination_indices() lists all index pairs", {
  expect_equal(unname(pairwise_combination_indices(3)), matrix(c(1, 1, 2, 2, 3, 3), ncol = 2))
  expect_equal(
    unname(pairwise_combination_indices(3, self = TRUE)),
    matrix(c(1, 1, 1, 2, 2, 3, 1, 2, 3, 2, 3, 3), ncol = 2)
  )
  expect_error(pairwise_combination_indices(0), "must be greater than 0")
})
