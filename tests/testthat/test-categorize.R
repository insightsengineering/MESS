test_that("categorize() with bare names equals table()", {
  expect_equal(categorize(esoph, alcgp, agegp), table(esoph$alcgp, esoph$agegp), ignore_attr = TRUE)
  expect_equal(categorize(esoph, alcgp), table(esoph$alcgp), ignore_attr = TRUE)
})

test_that("categorize() with a formula equals xtabs()", {
  expect_equal(categorize(esoph, ~ alcgp + agegp), xtabs(~ alcgp + agegp, data = esoph), ignore_attr = "call")
  expect_equal(categorize(esoph, ncases ~ alcgp), xtabs(ncases ~ alcgp, data = esoph), ignore_attr = "call")
})

test_that("categorize() errors for an unknown variable", {
  expect_error(categorize(esoph, nonexistentvariable), "not found")
})
