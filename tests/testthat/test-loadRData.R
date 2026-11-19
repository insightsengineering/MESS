test_that("loadRData returns the saved object regardless of its name", {
  path <- withr::local_tempfile(fileext = ".RData")
  my_object <- data.frame(x = 1:3, y = letters[1:3])
  save(my_object, file = path)

  expect_identical(loadRData(path), my_object)
})
