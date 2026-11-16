test_that("segregate.genes returns one column of genotypes per marker", {
  ped <- make_pedigree()
  withr::local_seed(42)
  res <- segregate.genes(ped, c(0.1, 0.3, 0.5))

  expect_equal(class(res), c("matrix", "array"))
  expect_equal(dim(res), c(14, 3))
  expect_equal(colnames(res), c("gene1", "gene2", "gene3"))
  expect_true(all(res %in% 0:2))
})

test_that("allele frequencies 0 and 1 give monomorphic markers", {
  ped <- make_pedigree()
  res <- segregate.genes(ped, c(0, 1))
  expect_equal(unname(res[, "gene1"]), rep(0, 14))
  expect_equal(unname(res[, "gene2"]), rep(2, 14))
})

test_that("offspring genotypes are Mendelian-consistent with their parents", {
  ped <- make_pedigree()
  withr::local_seed(1)
  res <- segregate.genes(ped, rep(0.5, 50))

  child <- ped$findex > 0
  dad <- res[ped$findex[child], , drop = FALSE]
  mom <- res[ped$mindex[child], , drop = FALSE]
  kids <- res[child, , drop = FALSE]
  # each parent transmits one allele: always if homozygous for it (2), never if 0
  expect_true(all(kids >= (dad == 2) + (mom == 2)))
  expect_true(all(kids <= (dad > 0) + (mom > 0)))
})

test_that("MZ twins receive identical genotypes", {
  ped <- kinship2::pedigree(
    id = 1:5,
    dadid = c(NA, NA, 1, 1, 1),
    momid = c(NA, NA, 2, 2, 2),
    sex = c("male", "female", "male", "male", "male"),
    relation = data.frame(id1 = 3, id2 = 4, code = 1)
  )
  withr::local_seed(123)
  res <- segregate.genes(ped, rep(0.5, 20))
  expect_equal(res[3, ], res[4, ])
})

test_that("a pedigree of founders only returns a data.frame", {
  ped <- kinship2::pedigree(id = 1:3, dadid = rep(NA, 3), momid = rep(NA, 3),
                            sex = c("male", "female", "male"))
  res <- segregate.genes(ped, c(0, 1))

  expect_s3_class(res, "data.frame")
  expect_equal(res, data.frame(gene1 = rep(0, 3), gene2 = rep(2, 3)))
})

test_that("invalid inputs raise informative errors", {
  ped <- make_pedigree()
  expect_error(segregate.genes(ped, c(-0.1, 0.5)), "minor allele frequencies must be in the range")
  expect_error(segregate.genes(ped, 1.5), "minor allele frequencies must be in the range")
  expect_error(segregate.genes(list(id = 1:3), 0.1), "requires a single pedigree as input")

  peds <- kinship2::pedigree(
    id = c(1, 2, 3, 11, 12, 13),
    dadid = c(NA, NA, 1, NA, NA, 11),
    momid = c(NA, NA, 2, NA, NA, 12),
    sex = c("male", "female", "male", "male", "female", "male"),
    famid = c("A", "A", "A", "B", "B", "B")
  )
  expect_error(segregate.genes(peds, 0.1), "requires a single pedigree as input")
})
