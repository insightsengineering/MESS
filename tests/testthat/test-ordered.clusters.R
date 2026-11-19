test_that("ordered.clusters is TRUE when equal ids are contiguous", {
  expect_true(ordered.clusters(c(1, 1, 1, 2, 2, 3)))
  expect_true(ordered.clusters(c(3, 3, 1, 2, 2)))
  expect_true(ordered.clusters(c("a", "a", "b")))
  expect_true(ordered.clusters(factor(c("b", "b", "a"))))
})

test_that("ordered.clusters is FALSE when an id reappears later", {
  expect_false(ordered.clusters(c(1, 2, 1)))
  expect_false(ordered.clusters(c(1, 1, 1, 2, 2, 3, 4, 1, 5, 5, 5)))
})

test_that("ordered.clusters is TRUE for vectors without duplicates", {
  expect_true(ordered.clusters(c(1, 2, 3)))
  expect_true(ordered.clusters(1))
  expect_true(ordered.clusters(numeric(0)))
})
