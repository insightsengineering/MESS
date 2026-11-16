library(mvtnorm)

test_that("rmvtnorm.pedigree returns a matrix of the correct dimensions", {
  mydata <- data.frame(id = 1:5,
                        dadid = c(NA, NA, 1, 1, 1),
                        momid = c(NA, NA, 2, 2, 2),
                        sex = c("male", "female", "male", "male", "male"),
                        famid = c(1, 1, 1, 1, 1))
  relation <- data.frame(id1 = c(3), id2 = c(4), famid = c(1), code = c(1))
  ped <- kinship2::pedigree(id = mydata$id, dadid = mydata$dadid, momid = mydata$momid,
                             sex = mydata$sex, relation = relation)

  set.seed(1)
  result <- rmvtnorm.pedigree(2, ped, h2 = .25)

  expect_true(is.matrix(result))
  expect_equal(nrow(result), length(ped$id))
  expect_equal(ncol(result), 2)
})

test_that("rmvtnorm.pedigree with h2=c2=d2=0 reduces to independent standard normals", {
  mydata <- data.frame(id = 1:5,
                        dadid = c(NA, NA, 1, 1, 1),
                        momid = c(NA, NA, 2, 2, 2),
                        sex = c("male", "female", "male", "male", "male"),
                        famid = c(1, 1, 1, 1, 1))
  relation <- data.frame(id1 = c(3), id2 = c(4), famid = c(1), code = c(1))
  ped <- kinship2::pedigree(id = mydata$id, dadid = mydata$dadid, momid = mydata$momid,
                             sex = mydata$sex, relation = relation)
  pedsize <- length(ped$id)

  set.seed(123)
  result <- rmvtnorm.pedigree(5, ped, h2 = 0, c2 = 0, d2 = 0)

  # With h2=c2=d2=0 the variance-covariance matrix used internally is the
  # identity, so the draw must match an independent rmvnorm call with the
  # same seed exactly.
  set.seed(123)
  expected <- t(mvtnorm::rmvnorm(5, mean = rep(0, pedsize), sigma = diag(pedsize)))

  expect_equal(result, expected)
})

test_that("rmvtnorm.pedigree validates its inputs", {
  mydata <- data.frame(id = 1:5,
                        dadid = c(NA, NA, 1, 1, 1),
                        momid = c(NA, NA, 2, 2, 2),
                        sex = c("male", "female", "male", "male", "male"),
                        famid = c(1, 1, 1, 1, 1))
  relation <- data.frame(id1 = c(3), id2 = c(4), famid = c(1), code = c(1))
  ped <- kinship2::pedigree(id = mydata$id, dadid = mydata$dadid, momid = mydata$momid,
                             sex = mydata$sex, relation = relation)

  expect_error(rmvtnorm.pedigree(2, ped, h2 = 0.6, c2 = 0.5),
               "input parameters wrong")
  expect_error(rmvtnorm.pedigree(2, ped, h2 = -0.1),
               "input parameters wrong")
  expect_error(rmvtnorm.pedigree(2, ped, d2 = 0.1),
               "cannot handle d2")
  expect_error(rmvtnorm.pedigree(2, list(id = 1:5)),
               "requires a single pedigree as input")
})

test_that("rmvt.pedigree returns a matrix of the correct dimensions", {
  mydata <- data.frame(id = 1:5,
                        dadid = c(NA, NA, 1, 1, 1),
                        momid = c(NA, NA, 2, 2, 2),
                        sex = c("male", "female", "male", "male", "male"),
                        famid = c(1, 1, 1, 1, 1))
  relation <- data.frame(id1 = c(3), id2 = c(4), famid = c(1), code = c(1))
  ped <- kinship2::pedigree(id = mydata$id, dadid = mydata$dadid, momid = mydata$momid,
                             sex = mydata$sex, relation = relation)

  set.seed(4)
  result <- rmvt.pedigree(2, ped, h2 = .25, df = 4)

  expect_true(is.matrix(result))
  expect_equal(nrow(result), length(ped$id))
  expect_equal(ncol(result), 2)
})

test_that("rmvt.pedigree with h2=c2=d2=0 reduces to independent central t draws", {
  mydata <- data.frame(id = 1:5,
                        dadid = c(NA, NA, 1, 1, 1),
                        momid = c(NA, NA, 2, 2, 2),
                        sex = c("male", "female", "male", "male", "male"),
                        famid = c(1, 1, 1, 1, 1))
  relation <- data.frame(id1 = c(3), id2 = c(4), famid = c(1), code = c(1))
  ped <- kinship2::pedigree(id = mydata$id, dadid = mydata$dadid, momid = mydata$momid,
                             sex = mydata$sex, relation = relation)
  pedsize <- length(ped$id)

  set.seed(99)
  result <- rmvt.pedigree(5, ped, h2 = 0, c2 = 0, d2 = 0, df = 4)

  set.seed(99)
  expected <- t(mvtnorm::rmvt(5, df = 4, sigma = diag(pedsize)))

  expect_equal(result, expected)
})

test_that("rmvt.pedigree validates its inputs", {
  mydata <- data.frame(id = 1:5,
                        dadid = c(NA, NA, 1, 1, 1),
                        momid = c(NA, NA, 2, 2, 2),
                        sex = c("male", "female", "male", "male", "male"),
                        famid = c(1, 1, 1, 1, 1))
  relation <- data.frame(id1 = c(3), id2 = c(4), famid = c(1), code = c(1))
  ped <- kinship2::pedigree(id = mydata$id, dadid = mydata$dadid, momid = mydata$momid,
                             sex = mydata$sex, relation = relation)

  expect_error(rmvt.pedigree(2, ped, h2 = 0.6, c2 = 0.5, df = 4),
               "input parameters wrong")
  expect_error(rmvt.pedigree(2, ped, h2 = -0.1, df = 4),
               "input parameters wrong")
  expect_error(rmvt.pedigree(2, ped, d2 = 0.1, df = 4),
               "cannot handle d2")
  expect_error(rmvt.pedigree(2, list(id = 1:5), df = 4),
               "requires a single pedigree as input")
})
