test_that("common.shared() gives a matrix of ones for a pedigree", {
  ped <- make_pedigree()
  result <- common.shared(ped)

  ids <- as.character(1:14)
  expect_identical(result, matrix(1, 14, 14, dimnames = list(ids, ids)))
})

test_that("common.shared() handles a single-person pedigree", {
  one <- kinship2::pedigree(1, NA, NA, "male")
  expect_identical(common.shared(one), matrix(1, 1, 1, dimnames = list("1", "1")))
})

test_that("common.shared() errors on duplicated ids", {
  ped <- make_pedigree()
  ped$id[2] <- ped$id[1]
  expect_error(common.shared(ped), "All id values must be unique")
})

test_that("common.shared() gives a block-diagonal sparse Matrix for a pedigreeList", {
  result <- common.shared(make_two_families())

  expect_s4_class(result, "Matrix")
  ids <- as.character(c(1:4, 11:14))
  expected <- kronecker(diag(2), matrix(1, 4, 4))
  dimnames(expected) <- list(ids, ids)
  expect_equal(as.matrix(result), expected)
})

test_that("common.shared() prefixes famid when ids repeat across families", {
  result <- common.shared(make_two_families(id = c(1:4, 1:4)))
  expect_equal(colnames(result), paste(rep(c("A", "B"), each = 4), 1:4, sep = "/"))
})
