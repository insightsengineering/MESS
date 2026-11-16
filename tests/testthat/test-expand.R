context("Test expand_table")

test_that("Expands a diagonal matrix to one row per diagonal entry", {
    result <- expand_table(diag(3))
    expect_equal(colnames(result), c("Var1", "Var2"))
    expect_equal(nrow(result), 3)
    expect_equal(as.character(result$Var1), c("A", "B", "C"))
    expect_equal(as.character(result$Var2), c("A", "B", "C"))
})

test_that("Row counts match the frequency counts in the matrix", {
    m <- matrix(c(2, 1, 3, 0, 0, 2), 3)
    result <- expand_table(m)
    expect_equal(nrow(result), sum(m))
    expect_equal(colnames(result), c("Var1", "Var2"))
})

test_that("A 3-dimensional table expands with one column per dimension", {
    result <- expand_table(UCBAdmissions)
    expect_equal(colnames(result), c("Admit", "Gender", "Dept"))
    expect_equal(nrow(result), sum(UCBAdmissions))
})

test_that("Non-matrix, non-table input raises an error", {
    expect_error(expand_table(1:5), "needs matrix or table as input")
})
