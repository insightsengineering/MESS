context("Test fitted.geese")

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
fit_geese  <- geepack::geese(y ~ x1 + age, id = id, data = dat, family = binomial, corstr = "exchangeable")

# NOTE (latent bugs, discovered while writing these tests): fitted.geese() is
# unable to successfully compute fitted values for *any* genuine
# geepack::geese() object, through two distinct, 100%-reproducible failure
# modes depending on whether `family` was supplied to geese():
#
# 1) When `family` is supplied explicitly (required for anything other than
#    the default gaussian family, e.g. the binomial fit used throughout this
#    package's fixtures), fitted.geese() builds `glmcall` from object$call and
#    nulls out id/jack/control/corstr/waves/zcor/std.err/scale.fix/
#    scale.value/z -- but *not* `family` (contrast with drop1.geese(), which
#    nulls the same set of fields plus `family`). The leftover `family =
#    binomial` argument is then passed through to `model.frame()`, which has
#    no `family` formal; model.frame.default() treats it as an extra
#    variable to evaluate and bind into the model frame, and fails because a
#    family function/closure (or a family *name* -- the lookup fails
#    similarly either way) is not a valid data-frame column type:
#      Error in model.frame.default(...): invalid type (closure) for variable '(family)'
#
# 2) When `family` is *not* supplied (relying on geese()'s default
#    family=gaussian), the leftover-argument problem above does not occur,
#    but fitted.geese() later calls `family(value)$linkinv(...)` on a
#    `value` object that is still of class "geese". There is no
#    `family.geese` S3 method anywhere in geepack, MESS, or base R, so this
#    always fails with:
#      Error in UseMethod("family"): no applicable method for 'family' applied to an object of class "geese"
#
# Both failure modes are reachable through the public fitted() generic on
# directly-returned output of geepack::geese(), independent of data size or
# correlation structure, so we document the actual (error) behavior here
# rather than asserting a working computation.

test_that("fitted() on a binomial geese fit fails because `family` leaks into model.frame() (latent bug)", {
    expect_error(fitted(fit_geese))
})

test_that("fitted() on a gaussian geese fit (default family, not passed explicitly) fails because there is no family.geese method (latent bug)", {
    y_cont <- -0.5 + 0.8 * x1 + 0.3 * age + u + rnorm(length(x1), sd = 0.5)
    dat_gauss <- dat
    dat_gauss$ycont <- y_cont
    # Deliberately do NOT pass family= here (relies on the gaussian default)
    # to isolate failure mode (2) from failure mode (1) above.
    fit_geese_gauss <- geepack::geese(ycont ~ x1 + age, id = id, data = dat_gauss, corstr = "exchangeable")

    expect_error(fitted(fit_geese_gauss))
})

test_that("fitted() on a gaussian geese fit *with* family supplied explicitly also fails (same leak as failure mode 1)", {
    y_cont <- -0.5 + 0.8 * x1 + 0.3 * age + u + rnorm(length(x1), sd = 0.5)
    dat_gauss <- dat
    dat_gauss$ycont <- y_cont
    fit_geese_gauss <- geepack::geese(ycont ~ x1 + age, id = id, data = dat_gauss,
                                       family = gaussian, corstr = "exchangeable")

    expect_error(fitted(fit_geese_gauss))
})
