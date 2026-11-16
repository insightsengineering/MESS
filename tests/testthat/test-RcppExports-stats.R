context("Test RcppExports stats functions")

##
## bin
##

test_that("bin computes correct bin counts for a hand-computable vector", {
    x <- c(1, 2, 5, 6, 9)
    res <- bin(x, 3)
    expect_equal(res$counts, c(2, 1, 1, 1))
    expect_equal(res$origin, 0)
    expect_equal(res$width, 3)
    expect_equal(res$missing, 0)
    expect_false(res$last_bin_is_missing)
})

test_that("bin origin shifts the bin boundaries and drops values below origin", {
    x <- c(1, 2, 5, 6, 9)
    res <- bin(x, 3, origin = 2)
    expect_equal(res$counts, c(1, 2, 1))
    expect_equal(res$origin, 2)
})

test_that("bin reproduces the documented example", {
    set.seed(1)
    x <- sample(10, 20, replace = TRUE)
    res <- bin(x, 15)
    expect_equal(res$counts, 20)
    expect_equal(res$missing, 0)
})

test_that("bin missinglast appends NA/NaN counts as a trailing bin", {
    x <- c(1, 2, NA, 3, NaN, 4)
    res <- bin(x, 2, missinglast = TRUE)
    expect_equal(res$counts, c(1, 2, 1, 2))
    expect_equal(res$missing, 2)
    expect_true(res$last_bin_is_missing)

    res2 <- bin(x, 2, missinglast = FALSE)
    expect_equal(res2$counts, c(1, 2, 1))
    expect_equal(res2$missing, 2)
    expect_false(res2$last_bin_is_missing)
})

test_that("bin errors for non-positive width", {
    expect_error(bin(1:5, 0))
    expect_error(bin(1:5, -1))
})

##
## cumsumbinning
##

test_that("cumsumbinning groups elements, starting a new group before the sum would exceed threshold", {
    x <- c(3, 4, 5, 12, 1, 5, 3)
    expect_equal(cumsumbinning(x, 10), c(1, 1, 2, 3, 4, 4, 4))
})

test_that("cumsumbinning with cutwhenpassed cuts the group right after the sum passes the threshold", {
    x <- c(3, 4, 5, 12, 1, 5, 3)
    expect_equal(cumsumbinning(x, 10, cutwhenpassed = TRUE), c(1, 1, 1, 2, 3, 3, 3))
})

test_that("cumsumbinning maxgroupsize caps the number of elements allowed per group", {
    x <- c(3, 4, 5, 12, 1, 5, 3)
    expect_equal(cumsumbinning(x, 10, maxgroupsize = 2), c(1, 1, 2, 3, 4, 4, 5))
    expect_equal(cumsumbinning(x, 10, cutwhenpassed = TRUE, maxgroupsize = 2), c(1, 1, 2, 2, 3, 3, 4))
})

##
## dCov / dCor
##

test_that("dCov and dCor reach dCor=1 for a perfectly linear relationship", {
    x <- matrix(1:10, ncol = 1)
    y <- matrix(seq(2, 20, by = 2), ncol = 1)
    expect_equal(dCor(x, y), 1, tolerance = 1e-6)
    expect_true(dCov(x, y) > 0)
})

test_that("dCov(x,x) and dCor(x,x) are non-negative with dCor(x,x) = 1", {
    x <- matrix(1:10, ncol = 1)
    expect_true(dCov(x, x) >= 0)
    expect_equal(dCor(x, x), 1, tolerance = 1e-6)
})

test_that("dCor stays within [0,1] and self-association exceeds unrelated random data", {
    set.seed(123)
    x <- matrix(1:10, ncol = 1)
    z <- matrix(rnorm(10), ncol = 1)
    dcor_self <- dCor(x, x)
    dcor_rand <- dCor(x, z)
    expect_true(dcor_rand >= -1e-8 && dcor_rand <= 1 + 1e-8)
    expect_true(dCov(x, x) > dCov(x, z))
    expect_true(dcor_self > dcor_rand)
})

test_that("dCov/dCor accept multi-column (10x2) matrices", {
    set.seed(7)
    x <- matrix(rnorm(20), ncol = 2)
    y <- matrix(rnorm(20), ncol = 2)
    expect_true(dCov(x, y) >= 0)
    dc <- dCor(x, y)
    expect_true(dc >= -1e-8 && dc <= 1 + 1e-8)
})

test_that("dCov/dCor error when matrices have differing numbers of rows", {
    expect_error(dCov(matrix(1:10, ncol = 1), matrix(1:8, ncol = 1)))
    expect_error(dCor(matrix(1:10, ncol = 1), matrix(1:8, ncol = 1)))
})

##
## pairwise_distance_correlation
##

test_that("pairwise_distance_correlation returns one non-negative value per column matching dCor^2", {
    set.seed(10)
    y <- rnorm(30)
    x <- matrix(rnorm(30 * 4), ncol = 4)
    pdc <- pairwise_distance_correlation(x, y)
    expect_equal(length(pdc), ncol(x))
    expect_true(all(pdc >= -1e-8))
    for (k in 1:4) {
        expect_equal(as.numeric(pdc[k]), dCor(x[, k, drop = FALSE], matrix(y, ncol = 1))^2, tolerance = 1e-6)
    }
})

test_that("pairwise_distance_correlation errors on mismatched row counts", {
    expect_error(pairwise_distance_correlation(matrix(1:10, ncol = 2), 1:3))
})

##
## filldown
##

test_that("filldown reproduces the first documented example", {
    a <- c(1:5, "Howdy", NA, NA, 2:3, NA)
    expect_equal(filldown(a),
                 c("1", "2", "3", "4", "5", "Howdy", "Howdy", "Howdy", "2", "3", "3"))
})

test_that("filldown reproduces the second documented example (leading NAs remain NA)", {
    expect_equal(filldown(c(NA, NA, NA, 3:5)), c(NA, NA, NA, 3, 4, 5))
})

test_that("filldown leaves a vector without any NA unchanged", {
    expect_equal(filldown(c(1, 2, 3)), c(1, 2, 3))
})

test_that("filldown keeps a leading NA as NA but fills later ones", {
    expect_equal(filldown(c(NA, 1, NA, 2)), c(NA, 1, 1, 2))
})

##
## hwe_frequencies
##

test_that("hwe_frequencies computes exact allele/genotype frequencies for a hand-traced example", {
    a1 <- c(1, 1, 2, 2, 1, 2)
    a2 <- c(1, 2, 2, 1, 1, 2)
    res <- hwe_frequencies(a1, a2)
    expect_named(res, c("allele_freq", "genotype_freq", "obs_genotype",
                         "available_genotypes", "unique_alleles"))
    expect_equal(res$allele_freq, c(0.5, 0.5))
    expect_equal(res$genotype_freq, c(0.25, 0.5, 0.25))
    expect_equal(res$obs_genotype, c(2L, 2L, 2L))
    expect_equal(res$available_genotypes, 6L)
    expect_equal(res$unique_alleles, 2L)
})

test_that("hwe_frequencies allele frequencies sum to 1 for a larger sample", {
    set.seed(5)
    al1 <- sample(1:3, 200, replace = TRUE)
    al2 <- sample(1:3, 200, replace = TRUE)
    res <- hwe_frequencies(al1, al2)
    expect_equal(sum(res$allele_freq), 1, tolerance = 1e-8)
    expect_equal(res$unique_alleles, 3L)
})

test_that("hwe_frequencies min_alleles forces a minimum allele_freq length", {
    set.seed(5)
    al1 <- sample(1:3, 200, replace = TRUE)
    al2 <- sample(1:3, 200, replace = TRUE)
    res <- hwe_frequencies(al1, al2, min_alleles = 5)
    expect_equal(length(res$allele_freq), 5)
    expect_equal(res$allele_freq[4:5], c(0, 0))
})

test_that("hwe_frequencies errors when allele1 and allele2 differ in length", {
    expect_error(hwe_frequencies(c(1, 2, 3), c(1, 2)))
})

##
## ks_cumtest
##

test_that("ks_cumtest returns an htest object with a valid p-value", {
    set.seed(1)
    x <- 1:6
    res <- ks_cumtest(x, B = 1000)
    expect_s3_class(res, "htest")
    expect_true(res$p.value >= 0 && res$p.value <= 1)
    expect_true(is.finite(res$statistic))
})

test_that("ks_cumtest result differs when a non-uniform prob vector is supplied", {
    set.seed(1)
    x <- 1:6
    res_uniform <- ks_cumtest(x, B = 1000)
    set.seed(1)
    res_nonuniform <- ks_cumtest(x, B = 1000, prob = c(5, 1, 1, 1, 1, 1))
    expect_false(isTRUE(all.equal(unname(res_uniform$statistic), unname(res_nonuniform$statistic))))
})

test_that("ks_cumtest errors when prob has a different length than x", {
    expect_error(ks_cumtest(1:6, B = 100, prob = c(1, 1, 1)))
})

##
## maximum_subarray
##

test_that("maximum_subarray matches the documented increasing-sequence example", {
    res <- maximum_subarray(1:4)
    expect_equal(res$sum, 10)
    expect_equal(res$start, 1)
    expect_equal(res$end, 4)
})

test_that("maximum_subarray matches the classic Kadane example", {
    res <- maximum_subarray(c(-2, 1, -3, 4, -1, 2, 1, -5, 4))
    expect_equal(res$sum, 6)
    expect_equal(res$start, 4)
    expect_equal(res$end, 7)
})

test_that("maximum_subarray handles purely negative vectors by returning the largest element", {
    res <- maximum_subarray(c(-5, -2, -8, -1, -9))
    expect_equal(res$sum, -1)
    expect_equal(res$start, 4)
    expect_equal(res$end, 4)
})

test_that("maximum_subarray handles a single-element vector", {
    res <- maximum_subarray(7)
    expect_equal(res$sum, 7)
    expect_equal(res$start, 1)
    expect_equal(res$end, 1)
})

##
## pairwise_Schur_product
##

test_that("pairwise_Schur_product computes exact pairwise column products without self", {
    X <- cbind(rep(1, 4), 1:4, 4:1)
    res <- pairwise_Schur_product(X)
    expect_equal(dim(res), c(4, 3))
    expected <- cbind(X[, 1] * X[, 2], X[, 1] * X[, 3], X[, 2] * X[, 3])
    expect_equal(unname(res), expected)
})

test_that("pairwise_Schur_product computes exact pairwise column products with self", {
    X <- cbind(rep(1, 4), 1:4, 4:1)
    res <- pairwise_Schur_product(X, self = TRUE)
    expect_equal(dim(res), c(4, 6))
    expected <- cbind(X[, 1] * X[, 1], X[, 1] * X[, 2], X[, 1] * X[, 3],
                       X[, 2] * X[, 2], X[, 2] * X[, 3], X[, 3] * X[, 3])
    expect_equal(unname(res), expected)
})

test_that("pairwise_Schur_product returns zero columns for a single-column matrix without self", {
    X <- matrix(1:4, ncol = 1)
    res <- pairwise_Schur_product(X)
    expect_equal(dim(res), c(4, 0))
})

##
## pairwise_combination_indices
##

test_that("pairwise_combination_indices enumerates all i<j pairs exactly once", {
    res <- pairwise_combination_indices(3)
    expect_equal(nrow(res), 3)
    expect_equal(unname(res), matrix(c(1, 1, 2, 2, 3, 3), ncol = 2))
    expect_true(all(res[, 1] < res[, 2]))
})

test_that("pairwise_combination_indices with self=TRUE also includes i==j pairs", {
    res <- pairwise_combination_indices(4, self = TRUE)
    expect_equal(nrow(res), 10)
    expect_true(all(res[, 1] <= res[, 2]))
    expect_equal(nrow(unique(res)), 10)
})

test_that("pairwise_combination_indices errors for n=0", {
    expect_error(pairwise_combination_indices(0))
})
