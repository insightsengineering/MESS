xml_data <- data.frame(a = 1:2, b = c("x", "y"))

test_that("write.xml writes one collapsed row element per observation", {
  path <- withr::local_tempfile(fileext = ".xml")

  expect_invisible(res <- write.xml(xml_data, file = path))
  expect_equal(res, 0)
  expect_equal(readLines(path), c(
    '<?xml version="1.0"?>',
    "<document>",
    "  <row><a>1</a><b>x</b></row>",
    "  <row><a>2</a><b>y</b></row>",
    "</document>"
  ))
})

test_that("collapse = FALSE writes each field on its own indented line", {
  path <- withr::local_tempfile(fileext = ".xml")
  write.xml(xml_data, file = path, collapse = FALSE)

  expect_equal(readLines(path), c(
    '<?xml version="1.0"?>',
    "<document>",
    "  <row>", "    <a>1</a>", "    <b>x</b>", "  </row>",
    "  <row>", "    <a>2</a>", "    <b>y</b>", "  </row>",
    "</document>"
  ))
})

test_that("factor and decimal columns are written as text", {
  path <- withr::local_tempfile(fileext = ".xml")
  write.xml(data.frame(f = factor("lvl"), n = 1.5), file = path)
  expect_equal(readLines(path)[3], "  <row><f>lvl</f><n>1.5</n></row>")
})

test_that("invalid inputs raise informative errors", {
  expect_error(write.xml(xml_data), "filename not specified")
  path <- withr::local_tempfile(fileext = ".xml")
  expect_error(write.xml(1:5, file = path), "data must be a data frame")
})
