# The smoke tests below muffle only that specific warning so that any
# other warning or error is still reported.
ignore_graphical_warnings <- function(expr) {
  withCallingHandlers(
    expr,
    warning = function(w) {
      if (grepl("does not name a graphical parameter", conditionMessage(w), fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

test_that("panel.hist and panel.r2 draw a pairs() plot", {
  withr::local_pdf(NULL)
  expect_no_error(ignore_graphical_params(
    pairs(
      ~ Ozone + Temp + Wind + Solar.R,
      data = airquality,
      diag.panel = panel.hist,
      upper.panel = panel.r2
    )
  ))
})

test_that("panel.hist and panel.r2 accept their optional arguments", {
  withr::local_pdf(NULL)
  x <- c(1, 3, 2, 5, 4, 6, 8, 7, 9, NA)
  y <- c(2, 1, 4, 3, 6, 5, 8, 7, 10, 9)

  plot.new()
  expect_no_error(ignore_b14(panel.hist(x, col.bar = "red", col = "blue")))
  plot.new()
  expect_no_error(ignore_b14(panel.r2(x, y, digits = 4, cex.cor = 2)))
})
