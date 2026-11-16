context("Test clipit")

test_that("Calls clipr::write_clip with the deparsed object (mocked clipboard)", {
    captured <- NULL
    testthat::local_mocked_bindings(
        write_clip = function(content, ...) { captured <<- content; invisible(NULL) },
        .package = "clipr"
    )

    x <- c(1, 2, 3)
    expect_null(clipit(x))
    expect_equal(captured, capture.output(dput(x)))
})

test_that("Copies the deparsed object to the clipboard when available", {
    skip_if_not(clipr::clipr_available(), "clipboard not available")

    x <- c(1, 2, 3)
    expect_null(clipit(x))
    expect_equal(clipr::read_clip(), capture.output(dput(x)))
})

test_that("Works with character vectors when clipboard is available", {
    skip_if_not(clipr::clipr_available(), "clipboard not available")

    x <- c("a", "b")
    clipit(x)
    expect_equal(clipr::read_clip(), capture.output(dput(x)))
})
