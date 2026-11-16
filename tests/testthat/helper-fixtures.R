# Shared fixtures for the MESS test suite.
#
# Fixtures are built by functions (not at file top level) so that every test
# creates its own data and random-number state never leaks between tests.

# 14-person, 3-generation pedigree used by the kinship / pedigree tests.
#
#   founders: 1, 2, 3, 4, 11, 12
#   5, 6      children of 1 x 2
#   7, 8      children of 3 x 4
#   9         child of 3 x 6
#   10        child of 7 x 2
#   13        child of 11 x 12
#   14        child of 10 x 13
make_pedigree <- function() {
  ped <- data.frame(
    id  = 1:14,
    dad = c(0, 0, 0, 0, 1, 1, 3, 3, 3, 7, 0, 0, 11, 10),
    mom = c(0, 0, 0, 0, 2, 2, 4, 4, 6, 2, 0, 0, 12, 13),
    sex = c(1, 2, 1, 2, 1, 2, 1, 2, 1, 1, 1, 2, 2, 2)
  )
  kinship2::pedigree(ped$id, ped$dad, ped$mom, ped$sex)
}

# Two nuclear families (parents + two children) as a pedigreeList.
# `id` lets the tests reuse the same ids in both families.
make_two_families <- function(id = c(1:4, 11:14)) {
  kinship2::pedigree(
    id = id,
    dadid = c(NA, NA, id[1], id[1], NA, NA, id[5], id[5]),
    momid = c(NA, NA, id[2], id[2], NA, NA, id[6], id[6]),
    sex = c(1, 2, 1, 2, 1, 2, 1, 2),
    famid = rep(c("A", "B"), each = 4)
  )
}

# Clustered binary outcome data for the GEE tests (drop1, QIC, geekin).
# 20 clusters of size 4 with a cluster-level random intercept.
make_gee_data <- function(seed = 1) {
  withr::local_seed(seed)
  n_clusters <- 20
  cluster_size <- 4
  id <- rep(seq_len(n_clusters), each = cluster_size)
  age <- rep(seq_len(cluster_size), times = n_clusters)
  x1 <- stats::rnorm(n_clusters * cluster_size)
  u <- rep(stats::rnorm(n_clusters, sd = 0.5), each = cluster_size)
  p <- stats::plogis(-0.5 + 0.8 * x1 + 0.3 * age + u)
  y <- stats::rbinom(length(p), 1, p)
  data.frame(id = id, y = y, x1 = x1, age = age)
}

# Poisson GEE on warpbreaks with 18 clusters of 3. The model call refers only
# to objects on the search path, so drop1()'s internal update() can refit it.
fit_warpbreaks_geeglm <- function() {
  geepack::geeglm(
    breaks ~ wool + tension, id = rep(1:18, each = 3), data = warpbreaks,
    family = poisson, corstr = "exchangeable"
  )
}

# Expected correlation for a nuclear family under the extended model:
#   spouses: rho, parent-child: theta * (1 + rho) / 2,
#   siblings: theta^2 * (1 + rho) / 2, all variances 1.
nuclear_expected <- function(rho, theta) {
  pc <- theta * (1 + rho) / 2
  sib <- theta^2 * (1 + rho) / 2
  m <- matrix(c(
    1,   rho, pc,  pc,
    rho, 1,   pc,  pc,
    pc,  pc,  1,   sib,
    pc,  pc,  sib, 1
  ), 4, 4, dimnames = list(as.character(1:4), as.character(1:4)))
  m
}

# Data for the feature.test() tests: y depends only on the first column of x.
# Tests pass a numeric `s`, which fixes lambda and skips the (random)
# cross-validation.
make_feature_data <- function() {
  withr::local_seed(42)
  x <- matrix(rnorm(20 * 5), nrow = 20)
  y <- rnorm(20, mean = 2 * x[, 1])
  list(x = x, y = y)
}

# A block-diagonal matrix of ones (one block per cluster) gives a single
# correlation parameter shared by all within-cluster pairs, i.e. geekin()
# reproduces geeglm() with an exchangeable working correlation.
exchangeable_varlist <- function(n_clusters = 20, cluster_size = 4) {
  kronecker(diag(n_clusters), matrix(1, cluster_size, cluster_size))
}

# Reference power for the two-sided test: probability under pa of the exact
# rejection region of stats::binom.test() at level 0.05.
binom_test_power <- function(n, p0, pa) {
  x <- 0:n
  reject <- vapply(x, function(k) binom.test(k, n, p0)$p.value <= 0.05, logical(1))
  sum(dbinom(x[reject], n, pa))
}
