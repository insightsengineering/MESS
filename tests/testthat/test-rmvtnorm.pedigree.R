# Nuclear family: parents 1 and 2 with three sons; 3 and 4 are MZ twins.
make_twin_pedigree <- function() {
  kinship2::pedigree(
    id = 1:5,
    dadid = c(NA, NA, 1, 1, 1),
    momid = c(NA, NA, 2, 2, 2),
    sex = c("male", "female", "male", "male", "male"),
    relation = data.frame(id1 = 3, id2 = 4, code = 1)
  )
}

# Additive relationship matrix (2 x kinship) of the twin pedigree, by hand:
# parent-child and full sibs 1/2, MZ twins 1, unrelated parents 0.
twin_relationship <- matrix(c(
  1.0, 0.0, 0.5, 0.5, 0.5,
  0.0, 1.0, 0.5, 0.5, 0.5,
  0.5, 0.5, 1.0, 1.0, 0.5,
  0.5, 0.5, 1.0, 1.0, 0.5,
  0.5, 0.5, 0.5, 0.5, 1.0
), nrow = 5, byrow = TRUE)

test_that("the twin pedigree has the expected relationship matrix", {
  expect_equal(unname(2 * kinship2::kinship(make_twin_pedigree())), twin_relationship)
})

test_that("rmvtnorm.pedigree draws from N(0, (1-h2-c2) I + h2 A + c2 J)", {
  ped <- make_twin_pedigree()
  h2 <- 0.3
  c2 <- 0.2
  sigma <- (1 - h2 - c2) * diag(5) + h2 * twin_relationship + c2

  withr::local_seed(123)
  res <- rmvtnorm.pedigree(4, ped, h2 = h2, c2 = c2)
  withr::local_seed(123)
  expected <- t(mvtnorm::rmvnorm(4, mean = rep(0, 5), sigma = sigma))

  expect_equal(dim(res), c(5, 4))
  expect_equal(unname(res), unname(expected))
})

test_that("rmvtnorm.pedigree with no genetic or shared effects gives independent normals", {
  withr::local_seed(1)
  res <- rmvtnorm.pedigree(3, make_twin_pedigree())
  withr::local_seed(1)
  expect_equal(res, t(mvtnorm::rmvnorm(3, mean = rep(0, 5), sigma = diag(5))))
})

test_that("rmvt.pedigree draws from a multivariate t with the same scale matrix", {
  ped <- make_twin_pedigree()
  h2 <- 0.3
  c2 <- 0.2
  sigma <- (1 - h2 - c2) * diag(5) + h2 * twin_relationship + c2

  withr::local_seed(99)
  res <- rmvt.pedigree(4, ped, h2 = h2, c2 = c2, df = 4)
  withr::local_seed(99)
  expected <- t(mvtnorm::rmvt(4, sigma = sigma, df = 4))

  expect_equal(dim(res), c(5, 4))
  expect_equal(unname(res), unname(expected))
})

test_that("both simulators validate their inputs", {
  ped <- make_twin_pedigree()
  for (sim in list(rmvtnorm.pedigree, rmvt.pedigree)) {
    expect_error(sim(2, ped, h2 = 0.6, c2 = 0.5), "input parameters wrong")
    expect_error(sim(2, ped, h2 = -0.1), "input parameters wrong")
    expect_error(sim(2, ped, d2 = 0.1), "cannot handle d2")
    expect_error(sim(2, list(id = 1:5)), "requires a single pedigree as input")
  }
})
