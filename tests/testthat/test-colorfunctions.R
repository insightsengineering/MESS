test_that("col.shade() mixes colours towards black", {
  expect_equal(col.shade("blue", 0), "#0000FFFF")
  expect_equal(col.shade("blue", 1), "#000000FF")
  expect_equal(col.shade(c("blue", "red"), 0.5), c("#00007FFF", "#7F0000FF"))
})

test_that("col.shade() keeps an existing alpha channel", {
  expect_equal(col.shade("#0000FF80", 0.5), "#00007F80")
})

test_that("col.tint() mixes colours towards white", {
  expect_equal(col.tint("blue", 0), "#FFFFFFFF")
  expect_equal(col.tint("blue", 1), "#0000FFFF")
  expect_equal(col.tint(c("blue", "red"), 0.5), c("#7F7FFFFF", "#FF7F7FFF"))
})

test_that("col.alpha() sets the alpha channel", {
  expect_equal(col.alpha("blue", 0), "#0000FF00")
  expect_equal(col.alpha("blue", 1), "#0000FFFF")
  expect_equal(col.alpha(c("blue", "red"), 0.5), c("#0000FF80", "#FF000080"))
  # an existing alpha channel is overwritten
  expect_equal(col.alpha("#0000FF20", 1), "#0000FFFF")
})

test_that("colour functions error when col is missing", {
  expect_error(col.shade(), "a vector of colours is missing")
  expect_error(col.tint(), "a vector of colours is missing")
  expect_error(col.alpha(), "a vector of colours is missing")
})

test_that("colour functions error for out-of-range arguments", {
  expect_error(col.shade("blue", -0.1), "shade must be between 0 and 1")
  expect_error(col.shade("blue", 1.1), "shade must be between 0 and 1")
  # col.tint's message wrongly says "shade" (B02), so only the range is matched
  expect_error(col.tint("blue", -0.1), "between 0 and 1")
  expect_error(col.tint("blue", 1.1), "between 0 and 1")
  expect_error(col.alpha("blue", -0.1), "alpha must be between 0 and 1")
  expect_error(col.alpha("blue", 1.1), "alpha must be between 0 and 1")
})
