# Poisson GEE on warpbreaks with 18 clusters of 3. The model call refers only
# to objects on the search path, so drop1()'s internal update() can refit it.
fit_warpbreaks_geeglm <- function() {
  geepack::geeglm(
    breaks ~ wool + tension, id = rep(1:18, each = 3), data = warpbreaks,
    family = poisson, corstr = "exchangeable"
  )
}

wald <- function(beta, vcov) drop(t(beta) %*% solve(vcov) %*% beta)

test_that("drop1() on geeglm gives Wald tests per term", {
  fit <- fit_warpbreaks_geeglm()
  out <- drop1(fit)
  b <- coef(fit)
  V <- fit$geese$vbeta

  expect_s3_class(out, c("anova", "data.frame"))
  expect_named(out, c("DF", "Wald", "Pr(>Chi)"))
  expect_equal(rownames(out), c("wool", "tension"))
  expect_equal(out$DF, c(1, 2))
  # 1 df term: same as the coefficient Wald test reported by geepack
  expect_equal(out["wool", "Wald"], summary(fit)$coefficients["woolB", "Wald"])
  # 2 df term: quadratic form of the two tension coefficients
  expect_equal(out["tension", "Wald"], wald(b[3:4], V[3:4, 3:4]))
  expect_equal(out[["Pr(>Chi)"]], pchisq(out$Wald, out$DF, lower.tail = FALSE))
})

test_that("drop1() on geeglm agrees with geepack's anova() for the last term", {
  withr::local_package("geepack")
  fit <- fit_warpbreaks_geeglm()
  expect_equal(drop1(fit)["tension", "Wald"], anova(fit)["tension", "X2"])
})

test_that("drop1() on geeglm uses the naive variance with method = 'naive'", {
  fit <- fit_warpbreaks_geeglm()
  b <- coef(fit)
  V <- fit$geese$vbeta.naiv

  out <- drop1(fit, method = "naive")
  expect_equal(out$Wald, c(wald(b[2], V[2, 2]), wald(b[3:4], V[3:4, 3:4])))
  expect_equal(drop1(fit, method = "sandwich"), drop1(fit, method = "robust"))
})

test_that("drop1() on geeglm restricts terms with scope", {
  fit <- fit_warpbreaks_geeglm()
  full <- drop1(fit)

  expect_equal(drop1(fit, scope = ~tension)$Wald, full["tension", "Wald"])
  expect_equal(drop1(fit, scope = "wool")$Wald, full["wool", "Wald"])
  expect_error(drop1(fit, scope = ~foo), "scope is not a subset of term labels")
})

test_that("drop1() on geeglm computes score tests", {
  fit <- fit_warpbreaks_geeglm()

  score <- drop1(fit, test = "score")
  expect_named(score, c("DF", "Score", "Pr(>Chi)"))
  expect_equal(score$Score, c(3.774668117, 10.086491034), tolerance = 1e-6) # regression values
  expect_equal(score[["Pr(>Chi)"]], pchisq(score$Score, c(1, 2), lower.tail = FALSE))

  sas <- drop1(fit, test = "sasscore")
  expect_named(sas, c("DF", "Score(SAS)", "Pr(>Chi)"))
  expect_equal(sas[["Score(SAS)"]], c(3.177161756, 7.627752460), tolerance = 1e-6) # regression values
})

test_that("drop1() score tests work when the data is a local variable", {
  skip_known_bug("B25")
  local_data <- warpbreaks
  fit <- geepack::geeglm(breaks ~ wool + tension, id = rep(1:18, each = 3), data = local_data, family = poisson)
  expect_named(drop1(fit, test = "score"), c("DF", "Score", "Pr(>Chi)"))
})

test_that("drop1() on geeglm with test = 'none' keeps the statistic", {
  skip_known_bug("B03")
  out <- drop1(fit_warpbreaks_geeglm(), test = "none")
  expect_named(out, c("DF", "Wald"))
})

test_that("drop1() on geem gives the same Wald tests as on geeglm", {
  g <- geeM::geem(
    breaks ~ wool + tension, id = rep(1:18, each = 3), data = warpbreaks,
    family = "poisson", corstr = "exchangeable"
  )
  out <- drop1(g)

  expect_named(out, c("DF", "Wald", "Pr(>Chi)"))
  expect_equal(out["wool", "Wald"], summary(g)$wald.test[2]^2)
  expect_equal(out["tension", "Wald"], wald(g$beta[3:4], g$var[3:4, 3:4]))
  expect_equal(drop1(g, method = "naive")$Wald[1], g$beta[2]^2 / g$naiv.var[2, 2])
  expect_equal(out$Wald, drop1(fit_warpbreaks_geeglm())$Wald, tolerance = 1e-4)
})

test_that("drop1() on geem computes score tests", {
  skip_known_bug("B26")
  g <- geeM::geem(breaks ~ wool + tension, id = rep(1:18, each = 3), data = warpbreaks, family = "poisson")
  expect_named(drop1(g, test = "score"), c("DF", "Score", "Pr(>Chi)"))
})

test_that("model.matrix() on geem returns the design matrix", {
  g <- geeM::geem(breaks ~ wool + tension, id = rep(1:18, each = 3), data = warpbreaks, family = "poisson")
  expect_identical(model.matrix(g), g$X)
  expect_equal(unname(model.matrix(g)), unname(model.matrix(~ wool + tension, warpbreaks)), ignore_attr = TRUE)
})

test_that("drop1() on geese gives Wald tests per term", {
  skip_known_bug("B04")
  gs <- geepack::geese(
    breaks ~ wool + tension, id = rep(1:18, each = 3), data = warpbreaks,
    family = poisson, corstr = "exchangeable"
  )
  expect_equal(drop1(gs)$Wald, drop1(fit_warpbreaks_geeglm())$Wald)
})
