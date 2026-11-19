test_that("ht prints head and tail separated by an ellipsis", {
  expect_output(
    ht(1:20),
    "[1] 1 2 3 4 5 6\n...\n[1] 15 16 17 18 19 20",
    fixed = TRUE
  )
})

test_that("ht: n and m control the number of head and tail elements", {
  expect_identical(
    capture.output(ht(1:20, n = 2, m = 3)),
    c("[1] 1 2", "...", "[1] 18 19 20")
  )
})

test_that("ht returns NULL invisibly when printing", {
  capture.output(vis <- withVisible(ht(1:20)))
  expect_null(vis$value)
  expect_false(vis$visible)
})

test_that("ht with returnList = TRUE returns head and tail without printing", {
  expect_silent(res <- ht(1:20, returnList = TRUE))
  expect_identical(res, list(head = 1:6, tail = 15:20))
})

test_that("ht works on data frames", {
  df <- data.frame(a = 1:10, b = letters[1:10])
  res <- ht(df, n = 2, m = 1, returnList = TRUE)
  expect_identical(res$head, df[1:2, ])
  expect_identical(res$tail, df[10, ])
})
