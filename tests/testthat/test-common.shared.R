test_that("common.shared.pedigree returns matrix of 1s with correct dimnames", {
  test1 <- data.frame(id  = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14),
                      mom  = c(0, 0, 0, 0, 2, 2, 4, 4, 6,  2,  0,  0, 12, 13),
                      dad  = c(0, 0, 0, 0, 1, 1, 3, 3, 3,  7,  0,  0, 11, 10),
                      sex  = c(1, 2, 1, 2, 1, 2, 1, 2, 1,  1,  1,  2,  2,  2))
  tped <- with(test1, kinship2::pedigree(id, dad, mom, sex))

  result <- common.shared(tped)

  expect_true(is.matrix(result))
  expect_equal(dim(result), c(14, 14))
  expect_equal(dimnames(result), list(as.character(tped$id), as.character(tped$id)))
  expect_true(all(result == 1))
})

test_that("common.shared.pedigree handles a size-1 pedigree", {
  one <- data.frame(id = 1, dadid = NA, momid = NA, sex = "male")
  pone <- with(one, kinship2::pedigree(id, dadid, momid, sex))

  result <- common.shared(pone)

  expect_equal(dim(result), c(1, 1))
  expect_equal(result[1, 1], 1)
  expect_equal(dimnames(result), list("1", "1"))
})

test_that("common.shared.pedigree errors on duplicated ids", {
  test1 <- data.frame(id  = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14),
                      mom  = c(0, 0, 0, 0, 2, 2, 4, 4, 6,  2,  0,  0, 12, 13),
                      dad  = c(0, 0, 0, 0, 1, 1, 3, 3, 3,  7,  0,  0, 11, 10),
                      sex  = c(1, 2, 1, 2, 1, 2, 1, 2, 1,  1,  1,  2,  2,  2))
  tped <- with(test1, kinship2::pedigree(id, dad, mom, sex))
  badped <- tped
  badped$id[2] <- badped$id[1]

  expect_error(common.shared(badped), "All id values must be unique")
})

test_that("common.shared.pedigreeList returns a block-diagonal Matrix", {
  fam <- data.frame(
    id    = c(1, 2, 3, 11, 12, 13),
    dadid = c(NA, NA, 1, NA, NA, 11),
    momid = c(NA, NA, 2, NA, NA, 12),
    sex   = c("male", "female", "male", "male", "female", "male"),
    famid = c("A", "A", "A", "B", "B", "B")
  )
  pedlist <- with(fam, kinship2::pedigree(id, dadid, momid, sex, famid = famid))
  expect_true(inherits(pedlist, "pedigreeList"))

  result <- common.shared(pedlist)

  expect_true(inherits(result, "Matrix"))
  expect_equal(dim(result), c(6, 6))
  expect_equal(dimnames(result), list(as.character(fam$id), as.character(fam$id)))

  mat <- as.matrix(result)
  # within-family block is all 1s
  expect_true(all(mat[c("1", "2", "3"), c("1", "2", "3")] == 1))
  expect_true(all(mat[c("11", "12", "13"), c("11", "12", "13")] == 1))
  # cross-family entries are 0
  expect_equal(mat["1", "11"], 0)
  expect_equal(mat["3", "13"], 0)
})
