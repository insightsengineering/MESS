context("Test ht")

test_that("Prints head and tail separated by an ellipsis", {
    output <- capture.output(ht(1:20))
    expect_equal(output, c("[1] 1 2 3 4 5 6", "...", "[1] 15 16 17 18 19 20"))
})

test_that("n and m control the number of head/tail elements shown", {
    output <- capture.output(ht(1:20, n = 2, m = 3))
    expect_equal(output, c("[1] 1 2", "...", "[1] 18 19 20"))
})

test_that("returnList=TRUE returns a list with head and tail, and prints nothing", {
    output <- capture.output(result <- ht(1:20, returnList = TRUE))
    expect_equal(output, character(0))
    expect_type(result, "list")
    expect_named(result, c("head", "tail"))
    expect_equal(result$head, head(1:20, 6))
    expect_equal(result$tail, tail(1:20, 6))
})

test_that("returnList=FALSE returns NULL invisibly", {
    capture.output(vis <- withVisible(ht(1:20)))
    expect_null(vis$value)
    expect_false(vis$visible)
})
