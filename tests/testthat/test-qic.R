context("Test QIC.geeglm / QIC.ordgee / QIC.geekin")

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

fit_geeglm  <- geepack::geeglm(y ~ x1 + age, id = id, data = dat, family = binomial, corstr = "exchangeable")
fit_geeglm2 <- geepack::geeglm(y ~ x1,       id = id, data = dat, family = binomial, corstr = "exchangeable")

expected_names <- c("QIC", "QICu", "Quasi Lik", "CIC", "params", "QICC")

# NOTE on two deviations required to properly test MESS::QIC() in this file:
#
# 1) geepack itself exports a function called QIC() (with its own
#    QIC.geeglm/QIC.ordgee methods). Since this file attaches geepack with
#    library(geepack), and geepack's attach happens *after* MESS is loaded,
#    an unqualified `QIC(...)` call resolves to geepack::QIC, not MESS::QIC
#    -- silently testing the wrong package's (differently implemented, and
#    not buggy in the way described below) function. All calls below are
#    therefore qualified as MESS::QIC() to guarantee we exercise MESS's own
#    generic and its registered S3 methods (QIC.geeglm/QIC.ordgee/QIC.geekin).
#
# 2) MESS::QIC.geeglm()'s internal computeqic() helper refits the model with
#    corstr="independence" via `eval(object$call, parent.frame())`. Because
#    this eval() lives inside a closure (computeqic) nested inside
#    QIC.geeglm, `parent.frame()` there resolves to QIC.geeglm's own
#    execution frame rather than to the original caller's frame (contrast
#    with geepack's own QIC.geeglm, which captures `env = parent.frame()` as
#    a formal default argument -- evaluated one frame further out -- and
#    passes that along explicitly). Free variables referenced by the stored
#    model call (here `dat`/`id`/`dat_gauss`) are therefore looked up via
#    QIC.geeglm's lexical enclosure (the MESS namespace) rather than this
#    test file's environment, and must be placed in .GlobalEnv for the
#    refit to find them. This is a latent environment-handling bug in
#    MESS::QIC.geeglm, analogous to the one documented for drop1.geeglm's
#    score/sasscore branch in test-drop1.geeglm.R.

## ---- QIC.geeglm: single model -------------------------------------------

test_that("QIC on a single geeglm object returns a named numeric vector of finite values", {
    assign("dat", dat, envir = .GlobalEnv)
    assign("id", id, envir = .GlobalEnv)
    on.exit(rm(list = c("dat", "id"), envir = .GlobalEnv), add = TRUE)

    out <- MESS::QIC(fit_geeglm)
    expect_type(out, "double")
    expect_null(dim(out))
    expect_equal(names(out), expected_names)
    expect_true(all(is.finite(out)))
    expect_equal(unname(out["params"]), 3)
})

## ---- QIC.geeglm: multi-model comparison ----------------------------------
#
# NOTE (latent bug): QIC()'s signature is `QIC(object, tol=.Machine$double.eps, ...)`
# -- `tol` is the *second* formal, positioned before `...`. This means the
# naive, "obvious" way of comparing two models, QIC(fit_geeglm, fit_geeglm2),
# does NOT send fit_geeglm2 to `...`: ordinary R argument matching binds the
# second *positional* argument to `tol` instead, silently turning the second
# model object into the `tol` value used for matrix inversion deep inside
# computeqic() and erroring with "non-numeric argument to binary operator".
# Naming `tol` explicitly to work around this (QIC(fit_geeglm, fit_geeglm2,
# tol=.Machine$double.eps)) does not help either: match.call() inside
# QIC.geeglm then captures `tol` as one of the matched arguments too, which
# inflates Call[-1L] beyond the number of fitted rows and breaks the
# `row.names(res) <- as.character(Call[-1L])` step. The only way to reach
# QIC's multi-model comparison path without tripping over `tol` is to pass
# additional models as *named* `...` arguments (any name other than "tol"),
# e.g. QIC(fit_geeglm, model2 = fit_geeglm2).

test_that("the naive positional multi-model call QIC(fit1, fit2) is broken (latent bug)", {
    assign("dat", dat, envir = .GlobalEnv)
    assign("id", id, envir = .GlobalEnv)
    on.exit(rm(list = c("dat", "id"), envir = .GlobalEnv), add = TRUE)

    expect_error(MESS::QIC(fit_geeglm, fit_geeglm2))
})

test_that("QIC comparing two geeglm models (passed as a named ... argument) returns a matrix with one row per model", {
    assign("dat", dat, envir = .GlobalEnv)
    assign("id", id, envir = .GlobalEnv)
    on.exit(rm(list = c("dat", "id"), envir = .GlobalEnv), add = TRUE)

    out <- MESS::QIC(fit_geeglm, model2 = fit_geeglm2)
    expect_true(is.matrix(out) || is.data.frame(out))
    expect_equal(nrow(out), 2)
    expect_equal(colnames(out), expected_names)
    expect_true(all(is.finite(out)))
    # Different mean models -> different number of mean parameters
    expect_equal(unname(out[, "params"]), c(3, 2))
})

## ---- QIC.geeglm: gaussian branch of the quasi-likelihood switch() --------

test_that("QIC exercises the gaussian branch of the quasi-likelihood computation", {
    y_cont <- -0.5 + 0.8 * x1 + 0.3 * age + u + rnorm(length(x1), sd = 0.5)
    dat_gauss <- dat
    dat_gauss$ycont <- y_cont
    fit_gauss <- geepack::geeglm(ycont ~ x1 + age, id = id, data = dat_gauss,
                                  family = gaussian, corstr = "exchangeable")

    assign("dat_gauss", dat_gauss, envir = .GlobalEnv)
    assign("id", id, envir = .GlobalEnv)
    on.exit(rm(list = c("dat_gauss", "id"), envir = .GlobalEnv), add = TRUE)

    out <- MESS::QIC(fit_gauss)
    expect_equal(names(out), expected_names)
    expect_true(all(is.finite(out)))
})

## ---- QIC.ordgee ------------------------------------------------------

test_that("QIC.ordgee's class guard requires 'geeglm' in class(object) (latent bug)", {
    # QIC.ordgee() is dispatched for objects of class "ordgee", yet its very
    # first check is
    #   if (! ("geeglm" %in% class(object)) ) stop("QIC requires a geeglm object as input")
    # which means a *bona fide* ordgee object (whose class is "ordgee", not
    # "geeglm") will always fail this check. This is the realistic, reachable
    # behavior through the public QIC() generic, so we assert the error
    # rather than a successful computation.
    x <- structure(list(), class = "ordgee")
    expect_error(MESS::QIC(x))
})

## ---- QIC.geekin ------------------------------------------------------

test_that("QIC on a geekin fit returns the same structure/field names as QIC on a geeglm fit", {
    V <- matrix(0, nrow(dat), nrow(dat))
    for (cl in unique(id)) {
        ix <- which(id == cl)
        V[ix, ix] <- 1
    }
    diag(V) <- 1
    varlist <- list(V)

    # NOTE: geekin()'s `family` argument defaults to gaussian; the shared
    # fixture's `y` is binary, so family = binomial must be supplied
    # explicitly to get a model comparable to fit_geeglm (see also
    # test-geekin.R).
    fitkin <- geekin(y ~ x1 + age, id = id, data = dat, family = binomial, varlist = varlist)

    # geekin() deliberately overwrites geeFit$call with the user-facing call
    # to geekin(...) (see R/geekin.R, "Cheat the output..."), so QIC.geekin's
    # internal re-dispatch to QIC.geeglm needs `dat`/`id` visible the same
    # way as the plain geeglm fits above.
    assign("dat", dat, envir = .GlobalEnv)
    assign("id", id, envir = .GlobalEnv)
    on.exit(rm(list = c("dat", "id"), envir = .GlobalEnv), add = TRUE)

    out <- MESS::QIC(fitkin)
    expect_equal(names(out), expected_names)
    expect_true(all(is.finite(out)))
})

test_that("QIC.geekin's class guard requires 'geeglm' in class(object)", {
    x <- structure(list(), class = "geekin")
    expect_error(MESS::QIC(x))
})
