# Nuclear family: father 1, mother 2, children 3 and 4
make_nuclear_family <- function() {
  kinship2::pedigree(1:4, c(0, 0, 1, 1), c(0, 0, 2, 2), c(1, 2, 1, 2))
}

# Expected correlation for a nuclear family under the extended model:
#   spouses: rho, parent-child: theta * (1 + rho) / 2,
#   siblings: theta^2 * (1 + rho) / 2, all variances 1.
nuclear_expected <- function(rho, theta) {
  pc <- theta * (1 + rho) / 2
  sib <- theta^2 * (1 + rho) / 2
  m <- matrix(c(
    1,   rho, pc,  pc,
    rho, 1,   pc,  pc,
    pc,  pc,  1,   sib,
    pc,  pc,  sib, 1
  ), 4, 4, dimnames = list(as.character(1:4), as.character(1:4)))
  m
}

test_that("extended.shared() matches the closed form for a nuclear family", {
  ped <- make_nuclear_family()

  expect_equal(extended.shared(ped), nuclear_expected(rho = 1, theta = 1))
  expect_equal(extended.shared(ped, rho = 0.5, theta = 0.5), nuclear_expected(0.5, 0.5))
  expect_equal(extended.shared(ped, rho = 0, theta = 1), nuclear_expected(0, 1))
})

test_that("extended.shared() returns a symmetric matrix for the 14-person pedigree", {
  result <- extended.shared(make_pedigree())

  expect_equal(dim(result), c(14, 14))
  expect_equal(dimnames(result), list(as.character(1:14), as.character(1:14)))
  expect_true(isSymmetric(result))
  # Individuals with two founder parents have variance 1. Individual 9 has a
  # founder father (3) and a non-founder mother (6) that are uncorrelated, so
  # the variance is 2 * (1/2)^2 = 0.5 (likewise 10). Individual 14 has
  # uncorrelated parents with variances 0.5 and 1: (0.5 + 1) / 4 = 0.375.
  expect_equal(unname(diag(result)), c(rep(1, 8), 0.5, 0.5, 1, 1, 1, 0.375))
})

test_that("extended.shared() handles a single-person pedigree", {
  one <- kinship2::pedigree(1, NA, NA, "male")
  expect_identical(extended.shared(one), matrix(1, 1, 1, dimnames = list("1", "1")))
})

test_that("extended.shared() handles a pedigree with a single non-founder", {
  skip_known_bug("B21")
  trio <- kinship2::pedigree(1:3, c(0, 0, 1), c(0, 0, 2), c(1, 2, 1))
  expect_equal(extended.shared(trio), nuclear_expected(1, 1)[1:3, 1:3])
})

test_that("extended.shared() errors on duplicated ids", {
  ped <- make_pedigree()
  ped$id[2] <- ped$id[1]
  expect_error(extended.shared(ped), "All id values must be unique")
})

test_that("extended.shared() gives a block-diagonal sparse Matrix for a pedigreeList", {
  result <- extended.shared(make_two_families())

  expect_s4_class(result, "Matrix")
  expected <- as.matrix(Matrix::bdiag(nuclear_expected(1, 1), nuclear_expected(1, 1)))
  ids <- as.character(c(1:4, 11:14))
  dimnames(expected) <- list(ids, ids)
  expect_equal(as.matrix(result), expected)
})

test_that("extended.shared() prefixes famid when ids repeat across families", {
  result <- extended.shared(make_two_families(id = c(1:4, 1:4)))
  expect_equal(colnames(result), paste(rep(c("A", "B"), each = 4), 1:4, sep = "/"))
})

test_that("extended.shared() passes rho and theta on for a pedigreeList", {
  skip_known_bug("B05")
  result <- extended.shared(make_two_families(), rho = 0.5, theta = 0.5)
  expect_equal(as.matrix(result)[1:4, 1:4], nuclear_expected(0.5, 0.5), ignore_attr = TRUE)
})
