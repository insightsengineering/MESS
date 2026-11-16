test_that("col.shade returns a valid color string", {
  newcol <- col.shade("blue")
  expect_true(is.character(newcol))
  expect_true(grepl("^#[0-9A-Fa-f]{8}$", newcol))
})

test_that("col.shade with shade=0 leaves RGB essentially unchanged", {
  expect_equal(col.shade("blue", 0), "#0000FFFF")
})

test_that("col.shade with shade=1 goes to black", {
  expect_equal(col.shade("blue", 1), "#000000FF")
})

test_that("col.shade is vectorized over multiple colors", {
  res <- col.shade(c("blue", "red"), 0.5)
  expect_length(res, 2)
  expect_true(all(grepl("^#[0-9A-Fa-f]{8}$", res)))
})

test_that("col.shade errors when col is missing", {
  expect_error(col.shade())
})

test_that("col.shade errors for out-of-range shade values", {
  expect_error(col.shade("blue", -0.1))
  expect_error(col.shade("blue", 1.1))
})

test_that("col.tint returns a valid color string", {
  newcol <- col.tint("blue")
  expect_true(is.character(newcol))
  expect_true(grepl("^#[0-9A-Fa-f]{8}$", newcol))
})

test_that("col.tint with tint=0 goes to white", {
  expect_equal(col.tint("blue", 0), "#FFFFFFFF")
})

test_that("col.tint with tint=1 leaves color unchanged", {
  expect_equal(col.tint("blue", 1), "#0000FFFF")
})

test_that("col.tint is vectorized over multiple colors", {
  res <- col.tint(c("blue", "red"), 0.5)
  expect_length(res, 2)
  expect_true(all(grepl("^#[0-9A-Fa-f]{8}$", res)))
})

test_that("col.tint errors when col is missing", {
  expect_error(col.tint())
})

test_that("col.tint errors for out-of-range tint values", {
  expect_error(col.tint("blue", -0.1))
  expect_error(col.tint("blue", 1.1))
})

test_that("col.alpha returns a valid color string", {
  newcol <- col.alpha("blue", .5)
  expect_true(is.character(newcol))
  expect_true(grepl("^#[0-9A-Fa-f]{8}$", newcol))
})

test_that("col.alpha with alpha=0 is fully transparent", {
  expect_equal(col.alpha("blue", 0), "#0000FF00")
})

test_that("col.alpha with alpha=1 is fully opaque", {
  expect_equal(col.alpha("blue", 1), "#0000FFFF")
})

test_that("col.alpha is vectorized over multiple colors", {
  res <- col.alpha(c("blue", "red"), 0.5)
  expect_length(res, 2)
  expect_true(all(grepl("^#[0-9A-Fa-f]{8}$", res)))
})

test_that("col.alpha errors when col is missing", {
  expect_error(col.alpha())
})

test_that("col.alpha errors for out-of-range alpha values", {
  expect_error(col.alpha("blue", -0.1))
  expect_error(col.alpha("blue", 1.1))
})
