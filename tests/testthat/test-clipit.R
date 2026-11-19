test_that("clipit() writes the dput() representation to the clipboard", {
  captured <- NULL
  local_mocked_bindings(
    write_clip = function(content, ...) {
      captured <<- content
      invisible(content)
    },
    .package = "clipr"
  )

  clipit(c(1, 2, 3))
  expect_equal(captured, "c(1, 2, 3)")

  clipit(c("a", "b"))
  expect_equal(captured, "c(\"a\", \"b\")")
})
