context("Test write.xml")

test_that("Writes a collapsed XML file with one row per observation", {
    df <- data.frame(a = 1:2, b = c("x", "y"))
    tf <- tempfile(fileext = ".xml")
    on.exit(unlink(tf))

    write.xml(df, file = tf)
    lines <- readLines(tf)

    expect_equal(lines[1], '<?xml version="1.0"?>')
    expect_equal(lines[2], "<document>")
    expect_equal(lines[length(lines)], "</document>")
    expect_true(any(grepl("<row><a>1</a><b>x</b></row>", lines, fixed = TRUE)))
    expect_true(any(grepl("<row><a>2</a><b>y</b></row>", lines, fixed = TRUE)))
})

test_that("collapse=FALSE writes each field on its own indented line", {
    df <- data.frame(a = 1:2, b = c("x", "y"))
    tf <- tempfile(fileext = ".xml")
    on.exit(unlink(tf))

    write.xml(df, file = tf, collapse = FALSE)
    content <- paste(readLines(tf), collapse = "\n")

    expect_true(grepl("<a>1</a>", content, fixed = TRUE))
    expect_true(grepl("    <b>x</b>", content, fixed = TRUE))
})

test_that("Missing filename raises an error", {
    df <- data.frame(a = 1:2)
    expect_error(write.xml(df), "filename not specified")
})

test_that("Non-data.frame input raises an error", {
    tf <- tempfile(fileext = ".xml")
    on.exit(unlink(tf))
    expect_error(write.xml(1:5, file = tf), "data must be a data frame")
})
