test_that("rud returns n group allocations from the available groups", {
  withr::local_seed(1)
  res <- rud(10, alpha = c(1, 1, 10), beta = 5)
  expect_type(res, "integer")
  expect_length(res, 10)
  expect_true(all(res %in% 1:3))
})

test_that("allocation follows the urn weights", {
  # A group with zero weight is never drawn while beta = 0 keeps it at zero
  expect_equal(rud(5, alpha = c(1, 0), beta = 0), rep(1L, 5))
  expect_equal(rud(5, alpha = c(0, 1), beta = 0), rep(2L, 5))

  # After the forced first draw of group 2, beta is added to the *other*
  # group, so the urn becomes c(1e6, 1) and group 1 is (almost surely) next
  withr::local_seed(1)
  expect_equal(rud(2, alpha = c(0, 1), beta = 1e6), c(2L, 1L))
})

test_that("the same seed gives the same allocation", {
  withr::local_seed(42)
  first <- rud(20)
  withr::local_seed(42)
  expect_equal(rud(20), first)
})

test_that("data.frame = TRUE returns ids, groups and treatment labels", {
  withr::local_seed(4)
  res <- rud(6, labels = c("Drug", "Placebo"), data.frame = TRUE, startid = 100)

  expect_s3_class(res, "data.frame")
  expect_named(res, c("id", "group", "treatment"))
  expect_equal(res$id, 100:105)
  expect_equal(res$treatment, c("Drug", "Placebo")[res$group])
})

test_that("invalid inputs raise informative errors", {
  expect_error(rud(5, alpha = 1), "length of at least 2")
  expect_error(rud(5, alpha = c(1, 1, 1), labels = c("A", "B")), "length of labels must be the same")
})
