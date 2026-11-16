test_that("age() matches the documented example", {
  born <- c("1971-08-18", "2000-02-28", "2001-12-20")
  expect_equal(age(born, "2016-08-28"), c(45, 16, 14))
})

test_that("age() is zero on the birth date and increments on the birthday", {
  expect_equal(age("2000-01-01", "2000-01-01"), 0)
  expect_equal(age("2000-05-10", c("2001-05-09", "2001-05-10")), c(0, 1))
})

test_that("age() handles a 29 February birth date", {
  expect_equal(age("2000-02-29", c("2001-02-28", "2001-03-01", "2004-02-29")), c(0, 1, 4))
})

test_that("age() accepts Date input", {
  expect_equal(age(as.Date("1990-05-10"), as.Date("2020-05-10")), 30)
})

test_that("age() errors when 'to' is before 'from' in an earlier year", {
  expect_error(age("2016-01-01", "2015-01-01"), "ages cannot be negative")
})

test_that("age() errors when 'to' is before 'from' within the same year", {
  expect_error(age("2016-06-01", "2016-01-01"), "ages cannot be negative")
})

test_that("age() errors when 'to' is before 'from' within the same year/month", {
  expect_error(age("2016-06-03", "2016-06-01"), "ages cannot be negative")
})
