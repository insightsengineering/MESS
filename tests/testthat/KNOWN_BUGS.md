# Known defects in MESS 0.6.0 found while writing the tests

Each defect below was reproduced against MESS 0.6.0 installed from this
source tree. Where the defect changes observable behaviour, the test suite
contains a test that asserts the **correct** behaviour and starts with
`skip_known_bug("Bxx")` (see `helper-known-bugs.R`); all such tests live in `test-known-bugs.R`. These tests have been
checked to fail for the stated reason when the skip line is removed.
When a defect is fixed upstream, delete the skip line and the test becomes
active.

IDs are not contiguous; they were assigned per review batch.

| ID | Defect | Source | Test status |
|----|--------|--------|-------------|
| B01 | `auc()` with `absolutearea = TRUE` is wrong for unsorted `x` | R/auc.R | skipped test |
| B02 | `col.tint()` error message refers to "shade" | R/colorfunctions.R | listed only |
| B03 | `drop1.geeglm()` / `drop1.geem()` with `test = "none"` drop the statistic column | R/drop1.geeglm.R | skipped test |
| B04 | `drop1.geese()` always errors | R/drop1.geeglm.R | skipped test |
| B05 | `extended.shared()` ignores `rho`/`theta` for a `pedigreeList` | R/extended.shared.R | skipped test |
| B06 | rnorm_perfect() multiplies the mean by sd | R/rnormperfect.R | skipped test |
| B07 | prepost.test() uses the treated-arm regression (ehat1) in the control-arm augmentation term | R/prepost.R | skipped test |
| B08 | screen_variables(method = "global-DPP") always errors | R/screener.R | skipped test |
| B09 | QIC.geeglm refit cannot find data defined in the caller's environment | R/qic.R | skipped test |
| B10 | QIC.ordgee is unreachable / broken | R/qic.R | skipped test |
| B11 | QIC(fit1, fit2) fails: second model binds to `tol` | R/qic.R | skipped test |
| B12 | fitted.geese() always errors | R/fitted.geese.R | skipped test |
| B13 | feature.test() errors whenever the lasso selects a variable | R/feature.test.R | skipped test |
| B14 | panel.hist() / panel.r2() warn on every call and do not restore par("usr") | R/panel.R | skipped test |
| B15 | lower.tri.vector() accepts non-square matrices | R/lower.tri.vector.R | skipped test |
| B16 | loadRData() fails when the saved object is named `filename` | R/loadRData.R | skipped test |
| B17 | gkgamma() input check is unreachable (dead code, no test) | R/gkgamma.R | listed only |
| B20 | `age()` returns negative ages when `to` is earlier than `from` in the same year | R/age.R | skipped test |
| B21 | `extended.shared()` fails for a pedigree with exactly one non-founder | R/extended.shared.R | skipped test |
| B22 | `ks_cumtest()` does not rescale `prob` to sum to 1 | src/kstest.cpp | skipped test |
| B23 | `maximum_subarray()` reads out of bounds for an empty vector | src/maximum_subarray.cpp | skipped test |
| B24 | `cumsumbinning()` starts at group 2 when the first value exceeds the threshold | src/cumsumbinning.cpp | skipped test |
| B25 | `drop1()` score tests fail when the model data is not on the search path | R/drop1.geeglm.R | skipped test |
| B26 | `drop1()` score tests on `geem` objects always error | R/scorefct.R | skipped test |
| B30 | power_binom_test() two-sided power exceeds the nominal level for symmetric p0 | R/power.binom.test.R | skipped test |
| B31 | power_binom_test() cannot solve for p0 | R/power.binom.test.R | skipped test |
| B32 | power_prop_test() cannot solve for sig.level | R/power.prop.test.R | skipped test |
| B33 | power_prop_test() one-sided power counts both tails | R/power.prop.test.R | skipped test |
| B34 | power_mcnemar_test() rejects psi = 1 although the message says "1 or greater" | R/power.mcnemar.test.R | skipped test |
| B35 | geekin() fails unless geepack is attached | R/geekin.R | skipped test |
| B36 | print.geekin(digits = ) permanently changes options("digits") | R/geekin.R | skipped test |
| B40 | screen_variables()$selected is not a list when all lambdas screen the same number of variables | R/screener.R | skipped test |
| B41 | screen_variables() documentation: `selected` holds the *discarded* variables (doc only, no skipped test) | R/screener.R | listed only |
| B42 | write.xml() writes bogus rows for a zero-row data frame | R/writexml.R | skipped test |
| B44 | earthquakes documentation lists 22 variables (doc only, no skipped test) | R/data.R | listed only |

## B01: `auc()` with `absolutearea = TRUE` is wrong for unsorted `x`
- Source: `R/auc.R:81` builds the zero-crossing points from the unsorted `x`/`y` (`x[idx]`, `y[idx+1]`). It should use the sorted `ox`/`oy`.
- Repro: `auc(c(4, 0, 1, 2, 3), c(0, 0, 0, 1, -1), absolutearea = TRUE)`
- Observed 1.75; expected 1.5 (same data sorted: `auc(0:4, c(0,0,1,-1,0), absolutearea = TRUE)`).
- Skipped test: `test-known-bugs.R`, "auc() with absolutearea = TRUE handles unsorted x values".

## B02: `col.tint()` error message refers to "shade"
- Source: `R/colorfunctions.R:53`, `stop("shade must be between 0 and 1")`.
- Repro: `col.tint("blue", 2)`
- Observed "shade must be between 0 and 1"; expected "tint must be between 0 and 1".
- Not skipped. `test-colorfunctions.R` "colour functions error for out-of-range arguments" matches only "between 0 and 1".

## B03: `drop1.geeglm()` / `drop1.geem()` with `test = "none"` drop the statistic column
- Source: `R/drop1.geeglm.R:88` (and `:193`). `switch(test, Wald=..., score=..., sasscore=...)` returns NULL for "none", so `aod[, NULL] <- score` adds nothing.
- Repro: `fit <- geepack::geeglm(breaks ~ wool + tension, id = rep(1:18, each = 3), data = warpbreaks, family = poisson); drop1(fit, test = "none")`
- Observed: only a `DF` column. Expected: `DF` and `Wald`, without p-values (as `drop1.geese` does).
- Skipped test: `test-known-bugs.R`, "drop1() on geeglm with test = 'none' keeps the statistic".

## B04: `drop1.geese()` always errors
- Source: `R/drop1.geeglm.R` `drop1.geese`. `drop.scope(object)` needs a `terms` component that `geese` objects lack. With an explicit scope, `eval(glmcall, parent.frame())` cannot rebuild the model frame, and `object$geese$vbeta` and `coef(object)` do not exist on a `geese` object (it uses `$beta` and `$vbeta`).
- Repro: `gs <- geepack::geese(breaks ~ wool + tension, id = rep(1:18, each = 3), data = warpbreaks, family = poisson); drop1(gs)`. This gives "no terms component nor attribute". `drop1(gs, scope = ~wool + tension)` gives "'data' must be of a vector type, was 'NULL'".
- Expected: Wald table equal to `drop1()` on the equivalent `geeglm` fit.
- Skipped test: `test-known-bugs.R`, "drop1() on geese gives Wald tests per term".

## B05: `extended.shared()` ignores `rho`/`theta` for a `pedigreeList`
- Source: `R/extended.shared.R:52`, `extended.shared(family)` is called without passing `rho` and `theta`.
- Repro: two nuclear families as a `pedigreeList`, then `extended.shared(pl, rho = 0.5, theta = 0.5)`.
- Observed: all within-family entries are 1, as with the defaults. Expected: the closed form (spouse 0.5, parent–child 0.375, siblings 0.1875).
- Skipped test: `test-known-bugs.R`, "extended.shared() passes rho and theta on for a pedigreeList".

## B06: rnorm_perfect() multiplies the mean by sd
- Source: R/rnormperfect.R:31 — `(qnorm(...) + mean)*sd`
- Repro: `mean(rnorm_perfect(30, mean = 10, sd = 2))`
- Observed: 20. Expected: 10 (values should be `mean + sd * qnorm(i/(n+1))`).
- Skipped test: `test-known-bugs.R` — "the sample is centred at mean and scaled by sd"

## B07: prepost.test() uses the treated-arm regression (ehat1) in the control-arm augmentation term
- Source: R/prepost.R:95, :99 (`(R-pihat0)*(1-Z)*ehat1/pihat0`) and :109 (`(ehat1-mu20)` in iFun2); should be ehat0.
- Only affects data with missing post values (with no missing data R - pihat0 ~ 0).
- Repro: documented Altman data with `post[c(2, 10, 12)] <- NA`; `prepost.test(bp_base, post, expo)$estimate`
- Observed: -6.591022948. Expected (Davidian, Tsiatis & Leon 2005 estimator, recomputed by hand with ehat0): -6.640460422.
- Skipped test: `test-known-bugs.R` — "missing post values are handled with the semiparametric estimator"

## B08: screen_variables(method = "global-DPP") always errors
- Source: R/screener.R:42 — `norm(x, 2)`; base::norm() needs `type = "2"` (character).
- Repro: `screen_variables(diag(3), c(3, 2, 1), lambda = 2.5, method = "global-DPP")`
- Observed: `Error in norm(x, 2): 'type' must be a character string`. Expected: list with DPP-screened indices.
- Skipped test: `test-known-bugs.R` — "global-DPP method applies the DPP criterion"

## B09: QIC.geeglm refit cannot find data defined in the caller's environment
- Source: R/qic.R:86 — `eval(object$call, parent.frame())` inside the nested `computeqic()` closure evaluates in QIC.geeglm's frame (enclosure: MESS namespace -> global env), not where the model was fitted. geepack::QIC captures the caller env and works.
- Repro: `f <- function() { dat <- geepack::ohio; fit <- geepack::geeglm(resp ~ age, id = id, data = dat, family = binomial); MESS::QIC(fit) }; f()`
- Observed: `Error in eval(mf, parent.frame()): object 'dat' not found`. Expected: same as `geepack::QIC(fit)`.
- Works only when the data is in the global env or referenced by a namespace-qualified expression (e.g. `data = geepack::ohio`) — the tests use the latter.
- Skipped test: `test-known-bugs.R` — "QIC works when the model data is a local variable"

## B10: QIC.ordgee is unreachable / broken
- Source: R/qic.R:136-173. `geepack::ordgee()` returns class "geese" (not "ordgee"), so the method is never dispatched; even if called, the guard at :143 requires class "geeglm" and :173 evaluates the object instead of its call.
- Repro: `MESS::QIC(geepack::ordgee(ordered(resp) ~ age, id = id, data = geepack::ohio))`
- Observed: `no applicable method for 'QIC' applied to an object of class "geese"`. Expected: QIC values for the ordinal GEE fit.
- Skipped test: `test-known-bugs.R` — "QIC works for an ordgee fit"

## B11: QIC(fit1, fit2) fails: second model binds to `tol`
- Source: R/qic.R:346 (generic) and :49 — `tol` is the 2nd formal, before `...`.
- Repro: `MESS::QIC(fit1, fit2)` (both geeglm fits)
- Observed: `Error in tol * Xsvd$d[1L]: non-numeric argument to binary operator`. Expected: 2 x 6 matrix. Works only as `QIC(fit1, model2 = fit2)`.
- Skipped test: `test-known-bugs.R` — "models can be compared by passing them positionally"

## B12: fitted.geese() always errors
- Source: R/fitted.geese.R:8 (the rebuilt `model.frame` call keeps `family=`), R/fitted.geese.R:30 (`family(value)` has no method for class "geese").
- Repro:
  ```r
  d <- data.frame(id = rep(1:10, each = 2), y = rep(0:1, 10), x = rnorm(20))
  fit <- geepack::geese(y ~ x, id = id, data = d, family = binomial)
  fitted(fit)   # Error in model.frame.default: invalid type (closure) for variable '(family)'
  fit2 <- geepack::geese(x ~ y, id = id, data = d)
  fitted(fit2)  # Error in UseMethod("family"): no applicable method for 'family' applied to an object of class "geese"
  ```
- Expected: the fitted values, i.e. `linkinv(X %*% beta)`, which equal `fitted()` of the equivalent geeglm fit.
- Skipped tests: `test-known-bugs.R`, "fitted.geese returns fitted values for a binomial geese fit" and "... for a gaussian geese fit".

## B13: feature.test() errors whenever the lasso selects a variable
- Source: R/feature.test.R:279 (`o.select[o.ols.order]`). `o.ols.order` is only assigned in the nothing-selected branch, because the assignment in the selected branch is commented out.
- Repro:
  ```r
  set.seed(42); x <- matrix(rnorm(100), 20); y <- rnorm(20, mean = 2 * x[, 1])
  feature.test(x, y, B = 5, s = 0.2, control = list(maxcores = 1))
  # Error: object 'o.ols.order' not found
  ```
- Expected: a result list with `ols.selected` and `lasso.selected` ordered by importance, plus permutation p-values.
- Note: the suspected `options(warn = -1)` leak on error was not reproduced.
- Skipped test: `test-known-bugs.R`, "feature.test selects the true predictor and returns permutation p-values".

## B14: panel.hist() / panel.r2() warn on every call and do not restore par("usr")
- Source: R/panel.R:25 and R/panel.R:61. `on.exit(par(usr))` passes an unnamed vector; it should be `on.exit(par(usr = usr))`.
- Repro:
  ```r
  pdf(NULL); plot.new(); plot.window(c(0, 10), c(0, 10)); u <- par("usr")
  panel.r2(1:9, c(2, 1, 4, 3, 6, 5, 8, 7, 9))   # Warning: argument 1 does not name a graphical parameter
  par("usr")                                    # 0 1 0 1, not u
  ```
- Expected: no warning, and `par("usr")` restored.
- Skipped test: `test-known-bugs.R`, "panel.hist and panel.r2 restore par('usr') without warnings". The smoke tests muffle only this specific warning.

## B15: lower.tri.vector() accepts non-square matrices
- Source: R/lower.tri.vector.R:26. `!is.matrix(x) & nrow(x) != ncol(x)` should use `|`.
- Repro: `lower.tri.vector(matrix(1:12, nrow = 3))` returns `2 3 6` and does not raise an error.
- Expected: error "Input must be a square matrix".
- Skipped test: `test-known-bugs.R`, "lower.tri.vector rejects a non-square matrix".

## B16: loadRData() fails when the saved object is named `filename`
- Source: R/loadRData.R:22. `load()` overwrites the local `filename` argument, and the object is then excluded by `ls() != "filename"`.
- Repro:
  ```r
  f <- tempfile(fileext = ".RData"); filename <- 1:3; save(filename, file = f)
  loadRData(f)   # Error in get(...): invalid first argument
  ```
- Expected: returns `1:3`. A fix is to `load()` into a new environment.
- Skipped test: `test-known-bugs.R`, "loadRData returns an object that is named 'filename'".

## B17: gkgamma() input check is unreachable (dead code, no test)
- Source: R/gkgamma.R:40. `is.matrix(as.matrix(x))` is always TRUE, so `stop("x must be a table or matrix")` can never run. Invalid input fails later with base-R errors.

## B20: `age()` returns negative ages when `to` is earlier than `from` in the same year
- Source: `R/age.R:26`. The negativity check is done on the year difference before the month/day adjustment.
- Repro: `age("2016-06-01", "2016-01-01")`
- Observed -1; expected the error "ages cannot be negative".
- Skipped test: `test-known-bugs.R`, "age() errors when 'to' is before 'from' within the same year".

## B21: `extended.shared()` fails for a pedigree with exactly one non-founder
- Source: `R/extended.shared.R:89`. `cbind(id$mindex, id$findex)[nonfounders,]` drops to a vector when there is one non-founder, so `spouses[,2:1]` (line 92) errors. Needs `drop = FALSE`.
- Repro: `extended.shared(kinship2::pedigree(1:3, c(0, 0, 1), c(0, 0, 2), c(1, 2, 1)))`
- Observed: "incorrect number of dimensions". Expected: a 3x3 matrix of ones (rho = theta = 1).
- Skipped test: `test-known-bugs.R`, "extended.shared() handles a pedigree with a single non-founder".

## B22: `ks_cumtest()` does not rescale `prob` to sum to 1
- Source: `src/kstest.cpp:66-75`. The normalisation of `prob` is commented out, although the documentation says "It will be scaled to sum to 1".
- Repro: `ks_cumtest(1:6, prob = c(5, 1, 1, 1, 1, 1))$statistic`
- Observed 9; expected 0.4571429, the same as with `prob = c(5, 1, 1, 1, 1, 1) / 10`. The p-value is also wrong.
- Skipped test: `test-known-bugs.R`, "ks_cumtest() rescales prob to sum to 1".

## B23: `maximum_subarray()` reads out of bounds for an empty vector
- Source: `src/maximum_subarray.cpp:27`, `double max_so_far = x[0];` without a length check.
- Repro: `maximum_subarray(numeric(0))`
- Observed: garbage (e.g. `sum = 4.69e-310`, start 1, end 1). Expected: an error.
- Skipped test: `test-known-bugs.R`, "maximum_subarray() rejects an empty vector".

## B24: `cumsumbinning()` starts at group 2 when the first value exceeds the threshold
- Source: `src/cumsumbinning.cpp:64`. When `cutwhenpassed = FALSE` the group counter is incremented before the first group has any members.
- Repro: `cumsumbinning(c(15, 1, 2), 10)`
- Observed `c(2, 3, 3)`; expected `c(1, 2, 2)`.
- Skipped test: `test-known-bugs.R`, "cumsumbinning() numbers groups from 1 when the first value exceeds the threshold".

## B25: `drop1()` score tests fail when the model data is not on the search path
- Source: `R/drop1.geeglm.R:69` (and `:174`). `update(object, ...)` evaluates the stored call in `drop1.geeglm`'s frame, whose enclosure is the MESS namespace. Data defined in the caller's local environment is not found. `stats::drop1.default` avoids this by evaluating in `environment(formula(object))`.
- Repro: `f <- function() { dd <- warpbreaks; m <- geepack::geeglm(breaks ~ wool + tension, id = rep(1:18, each = 3), data = dd, family = poisson); drop1(m, test = "score") }; f()`
- Observed: "object 'dd' not found". Expected: the score test table.
- Skipped test: `test-known-bugs.R`, "drop1() score tests work when the data is a local variable". The working score tests use the `warpbreaks` dataset, which is on the search path.

## B26: `drop1()` score tests on `geem` objects always error
- Source: `R/scorefct.R:34` uses `o$family$linkinv`, `o$id`, `o$corstr` and `o$geese$alpha`, which exist only on `geeglm` objects. A `geem` object stores the family in `$FunList` and the correlation in `$alpha`.
- Repro: `g <- geeM::geem(breaks ~ wool + tension, id = rep(1:18, each = 3), data = warpbreaks, family = "poisson"); drop1(g, test = "score")`
- Observed: "attempt to apply non-function". Expected: the score test table.
- Skipped test: `test-known-bugs.R`, "drop1() on geem computes score tests".

## B30: power_binom_test() two-sided power exceeds the nominal level for symmetric p0
- Source: R/power.binom.test.R:69 onwards (two-sided branch). The cumulative-sum search over sorted probabilities includes one value from a tied pair, so the rejection region then includes both tied values. The resulting region has size greater than sig.level.
- Repro:
  ```r
  power_binom_test(n = 50, p0 = 0.5, pa = 0.5)$power   # 0.0649 (size > 0.05)
  power_binom_test(n = 50, p0 = 0.5, pa = 0.75)$power  # 0.9713; binom.test rejection region gives 0.9449
  ```
  This also affects n = 60. Asymmetric p0 values agree exactly with the `binom.test` rejection region. The documented example (`## => power = 0.971`) shows the wrong value.
- Expected: power equal to the probability, under pa, of the exact `binom.test` rejection region at level 0.05.
- Skipped test: `test-known-bugs.R`, "power_binom_test two-sided test keeps its level for symmetric p0".

## B31: power_binom_test() cannot solve for p0
- Source: R/power.binom.test.R:117 and :120. The uniroot interval uses `p0` (which is NULL) where it should use `pa`.
- Repro: `power_binom_test(n = 50, pa = 0.45, power = 0.8, alternative = "greater")` gives "Error in uniroot: 'interval' must be a vector of length 2".
- Expected: p0 such that the power equals 0.8.
- Skipped test: `test-known-bugs.R`, "power_binom_test solves for p0".

## B32: power_prop_test() cannot solve for sig.level
- Source: R/power.prop.test.R:83. `extendInt = "upX"` is used, but n decreases as sig.level increases. Extending the interval beyond 1 also produces NaN warnings.
- Repro: `power_prop_test(n = 93, p1 = .5, p2 = .7, power = .8, sig.level = NULL)` gives "Error in uniroot: did not succeed extending the interval endpoints". `stats::power.prop.test` gives 0.05.
- Expected: about 0.05.
- Skipped test: `test-known-bugs.R`, "power_prop_test solves for sig.level".

## B33: power_prop_test() one-sided power counts both tails
- Source: R/power.prop.test.R:65. `pnorm(A) + pnorm(B, lower.tail = FALSE)` is used for both alternatives. With `tside = 1`, the wrong-direction tail is added, so the one-sided test has size 2 * sig.level.
- Repro: `power_prop_test(n = 50, p1 = .5, p2 = .5, alternative = "one.sided")$power` returns 0.10 (should be 0.05). For p1 = .5, p2 = .7 it gives 0.657313, versus 0.657229 from stats.
- Skipped test: `test-known-bugs.R`, "power_prop_test one-sided power matches stats::power.prop.test".

## B34: power_mcnemar_test() rejects psi = 1 although the message says "1 or greater"
- Source: R/power.mcnemar.test.R:55 (`any(psi <= 1)`).
- Repro: `power_mcnemar_test(n = 60, paid = .1, psi = 1)` gives "Error: psi must be 1 or greater ...".
- Expected: either accept psi = 1 (normal approximation power = sig.level / 2 = 0.025), or change the message to "greater than 1".
- Skipped test: `test-known-bugs.R`, "power_mcnemar_test accepts psi = 1 as its error message states".

## B35: geekin() fails unless geepack is attached
- Source: R/geekin.R:167. The call is rebuilt as `as.name("geeglm")` and evaluated in `parent.frame()`. geepack is only imported, so `geeglm` is not visible from user code.
- Repro (fresh session):
  ```r
  library(MESS)
  d <- data.frame(id = rep(1:10, each = 2), y = rnorm(20), x = rnorm(20))
  geekin(y ~ x, id = d$id, data = d, varlist = list(kronecker(diag(10), matrix(1, 2, 2))))
  # Error in geeglm(...): could not find function "geeglm"
  ```
- Expected: works without `library(geepack)`. A fix is to use `geepack::geeglm`.
- Skipped test: `test-known-bugs.R`, "geekin works when geepack is not attached". Inside testthat the test environment inherits MESS's imports, which hides the bug.
- Also relevant to `test-qic.R` ("QIC for a geekin fit ..."), which attaches geepack with `withr::local_package()` as the documented usage.

## B36: print.geekin(digits = ) permanently changes options("digits")
- Source: R/geekin.R:191 (`options(digits = digits)` with no restore).
- Repro: `print(fit, digits = 3); getOption("digits")` returns 3.
- Expected: the global option is unchanged.
- Skipped test: `test-known-bugs.R`, "print.geekin does not change the global digits option".

## B40: screen_variables()$selected is not a list when all lambdas screen the same number of variables
- Source: R/screener.R:50 — `apply(discard, 2, which)` simplifies to a vector/matrix.
- Repro: `screen_variables(diag(3), c(3, 2, 1), lambda = c(2.4, 2.5))$selected`
- Observed: `c(3L, 3L)` (integer vector; a matrix when lengths > 1). Expected: `list(3L, 3L)`. Also a single lambda returns a bare vector.
- Skipped test: `test-known-bugs.R` — "selected is a list even when every lambda selects the same number of variables"

## B41: screen_variables() documentation: `selected` holds the *discarded* variables (doc only, no skipped test)
- Source: R/screener.R:12 (@return says "indices of the selected variables") vs :48-50 (`discard <- outer(res, criterion, "<")`).
- Tests assert the actual behaviour (indices with |x'y| < criterion).

## B42: write.xml() writes bogus rows for a zero-row data frame
- Source: R/writexml.R:40 — `sapply(1:nrow(data), ...)` iterates over 1:0.
- Repro: `write.xml(data.frame(a = 1:2, b = c("x", "y"))[0, ], tf); readLines(tf)`
- Observed: rows `<a>NA</a><b>NA</b>` and `<a>character(0)</a>...`. Expected: header, `<document>`, `</document>` only. Fix: `seq_len(nrow(data))`.
- Skipped test: `test-known-bugs.R` — "a data frame without rows writes an empty document"

## B44: earthquakes documentation lists 22 variables (doc only, no skipped test)
- Source: R/data.R:435. The shipped data has 7 variables (time, latitude, longitude, depth, mag, place, type). test-data.R asserts the shipped structure.

## Observations (not defects)
- power_binom_test(power = ) solving for n: exact power is not monotone in n, so `uniroot` returns *an* n that achieves the requested power, not necessarily the smallest. For p0 = .3, pa = .5, power = .8 it returns 47, although n = 43 already gives 0.82. The test only checks that the returned n achieves the requested power.
- Solving power_binom_test for sig.level is a step function; the previous tests of it were removed as meaningless.
