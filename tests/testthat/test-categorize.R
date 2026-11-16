context("Test categorize")

test_that("Bare variable names produce a table with correct dimensions", {
    result <- esoph |> categorize(alcgp, agegp)
    expect_s3_class(result, "table")
    expect_equal(dim(result), c(4, 6))
})

test_that("Formula interface produces an xtabs object equivalent to the table form", {
    result_table <- esoph |> categorize(alcgp, agegp)
    result_formula <- esoph |> categorize(~ alcgp + agegp)
    expect_s3_class(result_formula, "xtabs")
    expect_equal(as.numeric(result_formula), as.numeric(result_table))
})

test_that("Referencing an undefined variable raises an error", {
    expect_error(esoph |> categorize(nonexistentvariable))
})
