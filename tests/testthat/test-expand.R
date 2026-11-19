test_that("expand_table() expands a matrix to one row per count", {
  m <- matrix(c(2, 1, 3, 0, 0, 2), 3)
  result <- expand_table(m)

  expect_named(result, c("Var1", "Var2"))
  expect_equal(nrow(result), sum(m))
  # Tabulating the expanded data recovers the original counts
  expect_equal(unclass(table(result)), m, ignore_attr = TRUE)
})

test_that("expand_table() keeps dimnames of a 2-way table", {
  x <- as.table(matrix(1:4, 2, dimnames = list(a = c("x", "y"), b = c("u", "v"))))
  result <- expand_table(x)

  expect_named(result, c("a", "b"))
  expect_equal(table(result), x, ignore_attr = "class")
})

test_that("expand_table() handles a 3-way table", {
  result <- expand_table(UCBAdmissions)

  expect_named(result, c("Admit", "Gender", "Dept"))
  expect_equal(nrow(result), sum(UCBAdmissions))
  expect_equal(table(result), UCBAdmissions, ignore_attr = "class")
})

test_that("expand_table() errors for non-matrix, non-table input", {
  expect_error(expand_table(1:5), "needs matrix or table as input")
})
