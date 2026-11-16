test_that("geekin with a block-of-ones varlist equals an exchangeable geeglm fit", {
  dat <- make_gee_data()
  V <- exchangeable_varlist()

  fit <- geekin(y ~ x1 + age, id = dat$id, data = dat, family = binomial, varlist = list(V))
  ref <- geepack::geeglm(y ~ x1 + age, id = id, data = dat, family = binomial, corstr = "exchangeable")

  expect_s3_class(fit, c("geekin", "geeglm"))
  expect_equal(coef(fit), coef(ref))
  expect_equal(unname(fit$geese$alpha), unname(ref$geese$alpha))
  expect_identical(fit$geese$model$corstr, "userdefined")
})

test_that("geekin accepts a single matrix as varlist", {
  dat <- make_gee_data()
  V <- exchangeable_varlist()

  fit_list <- geekin(y ~ x1 + age, id = dat$id, data = dat, family = binomial, varlist = list(V))
  fit_matrix <- geekin(y ~ x1 + age, id = dat$id, data = dat, family = binomial, varlist = V)
  expect_equal(coef(fit_matrix), coef(fit_list))
})

test_that("geekin drops observations with missing values like geeglm", {
  dat <- make_gee_data()
  dat$x1[c(3, 10)] <- NA
  V <- exchangeable_varlist()

  fit <- geekin(y ~ x1 + age, id = dat$id, data = dat, family = binomial, varlist = list(V))
  ref <- geepack::geeglm(y ~ x1 + age, id = id, data = na.omit(dat), family = binomial,
                         corstr = "exchangeable")

  expect_equal(coef(fit), coef(ref))
  expect_identical(as.vector(fit$na.action), c(3L, 10L))
})

test_that("geekin validates varlist and cluster ordering", {
  dat <- make_gee_data()
  V <- exchangeable_varlist()

  expect_error(
    geekin(y ~ x1 + age, id = dat$id, data = dat, family = binomial, varlist = "V"),
    "varlist must be a list"
  )

  shuffled <- dat[order(dat$age), ]
  expect_error(
    geekin(y ~ x1 + age, id = shuffled$id, data = shuffled, family = binomial, varlist = list(V)),
    "contiguous blocks"
  )
})

test_that("print.geekin shows the call, coefficients and correlation parameters", {
  dat <- make_gee_data()
  V <- exchangeable_varlist()
  fit <- geekin(y ~ x1 + age, id = dat$id, data = dat, family = binomial, varlist = list(V))

  out <- capture.output(res <- print(fit))
  expect_identical(res, fit)
  expect_true("Call:" %in% out)
  expect_true("Coefficients:" %in% out)
  expect_true("Estimated Correlation Parameters:" %in% out)
  expect_match(out, "Degrees of Freedom: 80 Total", fixed = TRUE, all = FALSE)
  expect_match(out, "Number of clusters:   20   Maximum cluster size: 4", fixed = TRUE, all = FALSE)
})
