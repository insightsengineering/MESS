context("Test rud")

test_that("output length equals n for the default documented example", {
    set.seed(1)
    res <- rud(5)
    expect_length(res, 5)
    expect_true(all(res %in% c(1, 2)))
})

test_that("custom alpha/beta weights are accepted and length is preserved", {
    set.seed(2)
    res <- rud(5, alpha = c(1, 1, 10), beta = 5)
    expect_length(res, 5)
    expect_true(all(res %in% c(1, 2, 3)))
})

test_that("data.frame=TRUE returns a data.frame with id/group/treatment columns of correct length and id respects startid", {
    set.seed(3)
    n <- 10
    startid <- 100
    res <- rud(n, data.frame = TRUE, startid = startid)

    expect_s3_class(res, "data.frame")
    expect_equal(names(res), c("id", "group", "treatment"))
    expect_equal(nrow(res), n)
    expect_equal(res$id, seq(startid, startid + n - 1))
})

test_that("custom labels appear in the treatment column", {
    set.seed(4)
    res <- rud(6, alpha = c(1, 1), labels = c("Drug", "Placebo"), data.frame = TRUE)

    expect_true(all(res$treatment %in% c("Drug", "Placebo")))
    expect_equal(res$treatment, c("Drug", "Placebo")[res$group])
})

test_that("alpha of length less than 2 raises an error", {
    expect_error(rud(5, alpha = c(1)), "length of at least 2")
})

test_that("labels length mismatched with alpha length raises an error", {
    expect_error(rud(5, alpha = c(1, 1, 1), labels = c("A", "B")), "length of labels must be the same")
})
