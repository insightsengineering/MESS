test_that("founder.shared.pedigree marks each individual and each mating pair", {
  ped <- make_pedigree()

  # Mating pairs (dad, mom) in make_pedigree(): 1x2, 3x4, 3x6, 7x2, 11x12, 10x13
  expected <- diag(14)
  pairs <- rbind(c(1, 2), c(3, 4), c(3, 6), c(7, 2), c(11, 12), c(10, 13))
  expected[pairs] <- 1
  expected[pairs[, 2:1]] <- 1
  dimnames(expected) <- list(as.character(1:14), as.character(1:14))

  expect_identical(founder.shared(ped), expected)
})

test_that("founder.shared.pedigree handles a single-person pedigree", {
  one <- kinship2::pedigree(id = 1, dadid = NA, momid = NA, sex = "male")
  expect_identical(founder.shared(one), matrix(1, 1, 1, dimnames = list("1", "1")))
})

test_that("founder.shared.pedigree errors on duplicated ids", {
  ped <- make_pedigree()
  ped$id[2] <- ped$id[1]
  expect_error(founder.shared(ped), "All id values must be unique")
})

test_that("founder.shared.pedigreeList returns a block-diagonal sparse matrix", {
  fam <- data.frame(
    id    = c(1, 2, 3, 11, 12, 13),
    dadid = c(NA, NA, 1, NA, NA, 11),
    momid = c(NA, NA, 2, NA, NA, 12),
    sex   = c("male", "female", "male", "male", "female", "male"),
    famid = c("A", "A", "A", "B", "B", "B")
  )
  pedlist <- with(fam, kinship2::pedigree(id, dadid, momid, sex, famid = famid))

  res <- founder.shared(pedlist)
  expect_s4_class(res, "Matrix")

  # Founders 1x2 (family A) and 11x12 (family B) are mating pairs
  block <- matrix(c(1, 1, 0, 1, 1, 0, 0, 0, 1), 3)
  expected <- as.matrix(Matrix::bdiag(block, block))
  dimnames(expected) <- list(as.character(fam$id), as.character(fam$id))
  expect_equal(as.matrix(res), expected)
})

test_that("founder.shared.pedigreeList uses famid/id names when ids repeat across families", {
  fam <- data.frame(
    id    = c(1, 2, 3, 1, 2, 3),
    dadid = c(NA, NA, 1, NA, NA, 1),
    momid = c(NA, NA, 2, NA, NA, 2),
    sex   = c("male", "female", "male", "male", "female", "male"),
    famid = c("A", "A", "A", "B", "B", "B")
  )
  pedlist <- with(fam, kinship2::pedigree(id, dadid, momid, sex, famid = famid))

  res <- founder.shared(pedlist)
  expect_identical(colnames(res), c("A/1", "A/2", "A/3", "B/1", "B/2", "B/3"))
})
