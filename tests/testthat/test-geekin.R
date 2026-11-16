context("Test geekin / print.geekin")

suppressPackageStartupMessages(library(geepack))

## ---- Shared fixture --------------------------------------------------
set.seed(1)
n_clusters <- 20
cluster_size <- 4
id <- rep(1:n_clusters, each = cluster_size)
age <- rep(1:cluster_size, times = n_clusters)
x1 <- rnorm(n_clusters * cluster_size)
u <- rep(rnorm(n_clusters, sd = 0.5), each = cluster_size)  # cluster-level random effect
p <- plogis(-0.5 + 0.8 * x1 + 0.3 * age + u)
y <- rbinom(length(p), 1, p)
dat <- data.frame(id = id, y = y, x1 = x1, age = age)

fit_geeglm <- geepack::geeglm(y ~ x1 + age, id = id, data = dat, family = binomial, corstr = "exchangeable")

# Block-diagonal "exchangeable-equivalent" correlation basis: one block of
# all-1s per cluster (off-diagonal elements scaled by a single alpha via
# geekin's userdefined zcor mechanism is mathematically equivalent to a
# standard exchangeable working correlation).
V <- matrix(0, nrow(dat), nrow(dat))
for (cl in unique(id)) {
    ix <- which(id == cl)
    V[ix, ix] <- 1
}
diag(V) <- 1

# NOTE (fixture deviation): geekin()'s `family` argument defaults to
# gaussian. The shared fixture's `y` is binary, so family = binomial must be
# supplied explicitly -- otherwise geekin() silently fits an identity-link
# (linear probability) model to the 0/1 outcome, which produces coefficients
# wildly different from fit_geeglm's logistic fit (e.g. opposite-signed
# intercept), rather than the "reasonably close" coefficients expected when
# comparing the same family/link with different correlation-structure
# parameterizations.
fit_geekin <- geekin(y ~ x1 + age, id = id, data = dat, family = binomial, varlist = list(V))

test_that("geekin returns an object of class geekin that also inherits geeglm", {
    expect_s3_class(fit_geekin, "geekin")
    expect_s3_class(fit_geekin, "geeglm")
})

test_that("coef(fit_geekin) is a named numeric vector close to coef(fit_geeglm)", {
    cf <- coef(fit_geekin)
    expect_type(cf, "double")
    expect_length(cf, 3)
    expect_equal(names(cf), c("(Intercept)", "x1", "age"))
    # The all-1s block-diagonal varlist (userdefined zcor) reproduces an
    # exchangeable working correlation, so the mean-model coefficients
    # should closely match the plain exchangeable geeglm fit.
    expect_equal(unname(cf), unname(coef(fit_geeglm)), tolerance = 1e-2)
})

test_that("varlist accepts a bare matrix identically to a single-element list", {
    fit_geekin_mat <- geekin(y ~ x1 + age, id = id, data = dat, family = binomial, varlist = V)
    expect_equal(coef(fit_geekin_mat), coef(fit_geekin))
})

test_that("geekin errors when varlist is neither a matrix nor a list", {
    expect_error(geekin(y ~ x1 + age, id = id, data = dat, family = binomial, varlist = 1:5),
                 "varlist must be a list")
    expect_error(geekin(y ~ x1 + age, id = id, data = dat, family = binomial, varlist = "foo"),
                 "varlist must be a list")
})

test_that("geekin errors when the clusters are not contiguous blocks", {
    # Interleave observations from different clusters by cluster-position so
    # that `id` is no longer made up of contiguous runs, while keeping
    # y/x1/age aligned with their original id for each row.
    idx <- order(rep(seq_len(cluster_size), times = n_clusters))
    dat_shuffled <- dat[idx, ]
    id_shuffled <- dat_shuffled$id
    expect_false(ordered.clusters(id_shuffled))

    expect_error(
        geekin(y ~ x1 + age, id = id_shuffled, data = dat_shuffled, family = binomial, varlist = list(V)),
        "contiguous blocks"
    )
})

test_that("print.geekin prints a Call: and Coefficients: section", {
    out <- capture.output(print(fit_geekin))
    expect_true(any(grepl("Call:", out, fixed = TRUE)))
    expect_true(any(grepl("Coefficients:", out, fixed = TRUE)))
})
