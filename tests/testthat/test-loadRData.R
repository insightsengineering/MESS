test_that("loadRData returns the saved object regardless of its name", {
  path <- withr::local_tempfile(fileext = ".RData")
  my_object <- data.frame(x = 1:3, y = letters[1:3])
  save(my_object, file = path)

  expect_identical(loadRData(path), my_object)
})

test_that("loadRData errors (with a warning) for a non-existent file", {
  path <- withr::local_tempfile(fileext = ".RData")
  expect_error(expect_warning(loadRData(path)), class = "error")
})
