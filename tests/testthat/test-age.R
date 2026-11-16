context("Test age")

test_that("Matches documented example ages", {
    born <- c("1971-08-18", "2000-02-28", "2001-12-20")
    check <- c("2016-08-28")
    expect_equal(age(born, check), c(45, 16, 14))
})

test_that("Age on the same date is zero", {
    expect_equal(age("2000-01-01", "2000-01-01"), 0)
})

test_that("Birthday not yet reached this year after a leap-year birth date", {
    expect_equal(age("2000-02-29", "2001-02-28"), 0)
})

test_that("Negative ages raise an error", {
    expect_error(age("2016-01-01", "2015-01-01"), "ages cannot be negative")
})
