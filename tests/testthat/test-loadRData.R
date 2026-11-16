context("Test loadRData")

test_that("Loads and returns the saved object regardless of its variable name", {
    tdir <- tempfile()
    dir.create(tdir)
    on.exit(unlink(tdir, recursive = TRUE))

    myobj <- data.frame(x = 1:3, y = letters[1:3])
    fp <- file.path(tdir, "test.RData")
    save(myobj, file = fp)

    result <- loadRData(fp)
    expect_identical(result, myobj)
})

test_that("Works when the saved object has a different name than the file", {
    tdir <- tempfile()
    dir.create(tdir)
    on.exit(unlink(tdir, recursive = TRUE))

    some_vector <- 1:10
    fp <- file.path(tdir, "other_name.RData")
    save(some_vector, file = fp)

    result <- loadRData(fp)
    expect_equal(result, 1:10)
})

test_that("A non-existent file raises an error (file connection cannot be opened)", {
    expect_error(
        suppressWarnings(loadRData(file.path(tempdir(), "does_not_exist_12345.RData"))),
        "cannot open the connection"
    )
})
