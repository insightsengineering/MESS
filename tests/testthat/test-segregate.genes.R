test_that("segregate.genes produces correctly shaped output with valid genotypes", {
  mydata <- data.frame(id = 1:5,
                        dadid = c(NA, NA, 1, 1, 1),
                        momid = c(NA, NA, 2, 2, 2),
                        sex = c("male", "female", "male", "male", "male"),
                        famid = c(1, 1, 1, 1, 1))
  relation <- data.frame(id1 = c(3), id2 = c(4), famid = c(1), code = c(1))
  ped <- kinship2::pedigree(id = mydata$id, dadid = mydata$dadid, momid = mydata$momid,
                             sex = mydata$sex, relation = relation)

  set.seed(42)
  res <- segregate.genes(ped, c(.1, .3, .5))

  expect_equal(nrow(res), length(ped$id))
  expect_equal(ncol(res), 3)
  expect_equal(colnames(res), c("gene1", "gene2", "gene3"))
  expect_true(all(unlist(res) %in% c(0, 1, 2)))
})

test_that("segregate.genes copies genotypes identically across MZ twins", {
  mydata <- data.frame(id = 1:5,
                        dadid = c(NA, NA, 1, 1, 1),
                        momid = c(NA, NA, 2, 2, 2),
                        sex = c("male", "female", "male", "male", "male"),
                        famid = c(1, 1, 1, 1, 1))
  relation <- data.frame(id1 = c(3), id2 = c(4), famid = c(1), code = c(1))
  ped <- kinship2::pedigree(id = mydata$id, dadid = mydata$dadid, momid = mydata$momid,
                             sex = mydata$sex, relation = relation)
  expect_equal(as.character(ped$relation$code), "MZ twin")

  set.seed(123)
  res <- segregate.genes(ped, c(.1, .3, .5))

  # individuals 3 and 4 are the MZ twin pair and must share genotypes at every gene
  expect_equal(as.numeric(res[3, ]), as.numeric(res[4, ]))
})

test_that("segregate.genes handles the all-founders edge case and returns a data.frame", {
  mydata <- data.frame(id = 1:3, dadid = c(NA, NA, NA), momid = c(NA, NA, NA),
                        sex = c("male", "female", "male"))
  ped0 <- kinship2::pedigree(id = mydata$id, dadid = mydata$dadid, momid = mydata$momid,
                              sex = mydata$sex)
  expect_true(all(kinship2::kindepth(ped0) == 0))

  set.seed(1)
  res0 <- segregate.genes(ped0, c(.2, .4))

  expect_true(is.data.frame(res0))
  expect_equal(dim(res0), c(3, 2))
  expect_true(all(unlist(res0) %in% c(0, 1, 2)))
})

test_that("segregate.genes errors on out-of-range minor allele frequencies", {
  mydata <- data.frame(id = 1:5,
                        dadid = c(NA, NA, 1, 1, 1),
                        momid = c(NA, NA, 2, 2, 2),
                        sex = c("male", "female", "male", "male", "male"),
                        famid = c(1, 1, 1, 1, 1))
  relation <- data.frame(id1 = c(3), id2 = c(4), famid = c(1), code = c(1))
  ped <- kinship2::pedigree(id = mydata$id, dadid = mydata$dadid, momid = mydata$momid,
                             sex = mydata$sex, relation = relation)

  expect_error(segregate.genes(ped, c(-0.1, 0.5)),
               "minor allele frequencies must be in the range")
  expect_error(segregate.genes(ped, c(1.5, 0.5)),
               "minor allele frequencies must be in the range")
})

test_that("segregate.genes errors when given a non-pedigree object", {
  expect_error(segregate.genes(list(id = 1:3), c(0.1)),
               "requires a single pedigree as input")

  fam <- data.frame(
    id    = c(1, 2, 3, 11, 12, 13),
    dadid = c(NA, NA, 1, NA, NA, 11),
    momid = c(NA, NA, 2, NA, NA, 12),
    sex   = c("male", "female", "male", "male", "female", "male"),
    famid = c("A", "A", "A", "B", "B", "B")
  )
  pedlist <- with(fam, kinship2::pedigree(id, dadid, momid, sex, famid = famid))
  expect_true(inherits(pedlist, "pedigreeList"))

  expect_error(segregate.genes(pedlist, c(0.1)),
               "requires a single pedigree as input")
})
