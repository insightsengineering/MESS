context("Test drop1.geeglm / drop1.geem / drop1.geese / model.matrix.geem")

suppressPackageStartupMessages(library(geepack))
suppressPackageStartupMessages(library(geeM))

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
fit_geem   <- geeM::geem(y ~ x1 + age, id = id, data = dat, family = "binomial", corstr = "exchangeable")

## ---- drop1 on geeglm objects ------------------------------------------

test_that("drop1 on a geeglm object returns an anova/data.frame with one row per term", {
    out <- drop1(fit_geeglm)
    expect_s3_class(out, "anova")
    expect_s3_class(out, "data.frame")
    expect_equal(sort(rownames(out)), sort(c("x1", "age")))
    expect_true(all(c("DF", "Wald", "Pr(>Chi)") %in% colnames(out)))
    expect_true(all(is.finite(out$Wald)))
    expect_true(all(out[["Pr(>Chi)"]] >= 0 & out[["Pr(>Chi)"]] <= 1))
})

test_that("drop1 with test='score' and test='sasscore' exercise the internal score test paths", {
    # drop1.geeglm()'s score/sasscore branch calls update(object, ...), which
    # (per base R's update.default) re-evaluates the stored model call via
    # eval(call, parent.frame()) -- i.e. in drop1.geeglm's own execution
    # frame. Free variables in that call (here `dat` and `id`) are then
    # resolved through drop1.geeglm's *lexical* enclosure (the MESS
    # namespace) rather than through this test file's environment, so they
    # must temporarily be made visible in the global environment for the
    # update() call inside drop1() to find them.
    assign("dat", dat, envir = .GlobalEnv)
    assign("id", id, envir = .GlobalEnv)
    on.exit({
        rm(list = c("dat", "id"), envir = .GlobalEnv)
    }, add = TRUE)

    out_score <- drop1(fit_geeglm, test = "score")
    expect_s3_class(out_score, "anova")
    expect_true("Score" %in% colnames(out_score))
    expect_true(all(is.finite(out_score$Score)))

    out_sas <- drop1(fit_geeglm, test = "sasscore")
    expect_s3_class(out_sas, "anova")
    expect_true("Score(SAS)" %in% colnames(out_sas))
    expect_true(all(is.finite(out_sas[["Score(SAS)"]])))
})

test_that("method='robust' and method='sandwich' give identical results, method='naive' differs", {
    out_robust   <- drop1(fit_geeglm, method = "robust")
    out_sandwich <- drop1(fit_geeglm, method = "sandwich")
    out_naive    <- drop1(fit_geeglm, method = "naive")

    expect_equal(out_robust, out_sandwich)
    expect_false(isTRUE(all.equal(out_robust$Wald, out_naive$Wald)))
})

test_that("explicit scope argument restricts the terms considered for dropping", {
    out <- drop1(fit_geeglm, scope = ~age)
    expect_equal(rownames(out), "age")
    expect_true(all(c("DF", "Wald", "Pr(>Chi)") %in% colnames(out)))
})

test_that("drop1.geeglm's internal class guard is unreachable via normal S3 dispatch on the generic", {
    # drop1.geeglm() contains an explicit
    #   stop("Presently drop1.geeglm only works for geeglm or geem objects")
    # guarded behind `if (! any(c("geeglm", "geem") %in% class(object)))`.
    # That branch can only execute if drop1.geeglm() is itself invoked, which
    # requires class(object) to contain "geeglm" in the first place (for the
    # generic drop1() to dispatch to this method) -- but then the guard
    # condition is automatically false. There is therefore no way to trigger
    # this specific stop() through the public drop1() generic; we only verify
    # here that an unrelated object dispatches elsewhere (drop1.default /
    # an error about no applicable method) rather than into MESS's method.
    not_a_gee <- structure(list(), class = "data.frame")
    expect_error(drop1(not_a_gee))
})

## ---- drop1 on geem objects --------------------------------------------

test_that("drop1 on a geem object works with the default Wald test", {
    out <- drop1(fit_geem)
    expect_s3_class(out, "anova")
    expect_equal(sort(rownames(out)), sort(c("x1", "age")))
    expect_true(all(c("DF", "Wald", "Pr(>Chi)") %in% colnames(out)))
    expect_true(all(is.finite(out$Wald)))
})

test_that("drop1 on a geem object with method='naive' runs and differs from the default", {
    out_default <- drop1(fit_geem)
    out_naive   <- drop1(fit_geem, method = "naive")
    expect_s3_class(out_naive, "anova")
    expect_false(isTRUE(all.equal(out_default$Wald, out_naive$Wald)))
})

test_that("model.matrix on a geem object returns the stored design matrix", {
    mm <- model.matrix(fit_geem)
    expect_equal(mm, fit_geem$X)
    expect_equal(nrow(mm), nrow(dat))
})

## ---- drop1 on geese objects --------------------------------------------
#
# NOTE (latent bug, discovered while writing these tests): drop1.geese()
# always fails on a genuine geepack::geese() object.
#   * With the default (missing) `scope` argument it calls drop.scope(object),
#     which in turn calls terms(object); a plain "geese" object has no
#     $terms component/attribute, so this errors with
#     "no terms component nor attribute" before drop1.geese's own code runs.
#   * Even when `scope` is supplied explicitly (bypassing drop.scope()), the
#     per-term loop uses `coef(object)[ii]` and `object$geese$vbeta[ii,ii]`.
#     Those accessors are only valid for a *geeglm* fit (which stores its
#     geese fit as a $geese sub-list); a bare `geese`-class object has no
#     $coefficients (so coef() returns NULL via coef.default) and no nested
#     $geese list (so object$geese$vbeta is NULL), which fails inside solve().
# Both failure modes are 100% reproducible through the public drop1()
# generic, independent of `test`/`method`, so we document the actual
# (error) behavior rather than asserting a working result.
test_that("drop1 on a real geese object fails with default scope (latent bug: drop.scope needs terms)", {
    expect_error(drop1(fit_geese))
})

test_that("drop1 on a real geese object fails even with an explicit scope (latent bug: coef()/geese$vbeta mismatch)", {
    expect_error(drop1(fit_geese, scope = ~x1 + age))
    expect_error(drop1(fit_geese, scope = ~x1 + age, test = "none"))
    expect_error(drop1(fit_geese, scope = ~x1 + age, method = "naive"))
})
