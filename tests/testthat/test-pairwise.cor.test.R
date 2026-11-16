make_grouped_data <- function() {
  withr::local_seed(42)
  data.frame(x = rnorm(30), g = factor(rep(c("A", "B", "C"), each = 10)))
}

test_that("pairwise.cor.test p-values equal adjusted cor.test p-values", {
  d <- make_grouped_data()
  xa <- d$x[d$g == "A"]
  xb <- d$x[d$g == "B"]
  xc <- d$x[d$g == "C"]
  raw <- c(cor.test(xb, xa)$p.value, cor.test(xc, xa)$p.value, cor.test(xc, xb)$p.value)

  res <- pairwise.cor.test(d$x, d$g)
  expect_s3_class(res, "pairwise.htest")
  expect_identical(res$p.adjust.method, "holm")
  expect_identical(res$method, "Pearson's product-moment correlation")
  expect_identical(res$data.name, "d$x and d$g")

  adjusted <- p.adjust(raw, "holm")
  expect_equal(res$p.value["B", "A"], adjusted[1])
  expect_equal(res$p.value["C", "A"], adjusted[2])
  expect_equal(res$p.value["C", "B"], adjusted[3])
  expect_identical(res$p.value["B", "B"], NA_real_)
})

test_that("pairwise.cor.test honours p.adjust.method", {
  d <- make_grouped_data()
  raw <- pairwise.cor.test(d$x, d$g, p.adjust.method = "none")$p.value
  bonf <- pairwise.cor.test(d$x, d$g, p.adjust.method = "bonferroni")

  expect_identical(bonf$p.adjust.method, "bonferroni")
  expect_equal(bonf$p.value, pmin(3 * raw, 1))
})

test_that("pairwise.cor.test supports kendall and spearman and passes ... to cor.test", {
  d <- make_grouped_data()
  xa <- d$x[d$g == "A"]
  xb <- d$x[d$g == "B"]

  kendall <- pairwise.cor.test(d$x, d$g, method = "kendall", p.adjust.method = "none")
  expect_identical(kendall$method, "Kendall's rank correlation tau")
  expect_equal(kendall$p.value["B", "A"], cor.test(xb, xa, method = "kendall")$p.value)

  spearman <- pairwise.cor.test(d$x, d$g, method = "spearman", p.adjust.method = "none")
  expect_identical(spearman$method, "Spearman's rank correlation rho")
  expect_equal(spearman$p.value["B", "A"], cor.test(xb, xa, method = "spearman")$p.value)

  greater <- pairwise.cor.test(d$x, d$g, p.adjust.method = "none", alternative = "greater")
  expect_equal(greater$p.value["B", "A"], cor.test(xb, xa, alternative = "greater")$p.value)
})

test_that("pairwise.cor.test returns NA with a warning for unequal group sizes", {
  x <- c(1, 2, 3, 4, 5, 2, 4, 6, 8, 10, 12, 14)
  g <- rep(c("A", "B"), c(5, 7))

  expect_warning(res <- pairwise.cor.test(x, g), "Group sizes are not identical")
  expect_identical(res$p.value["B", "A"], NA_real_)
})

test_that("pairwise.cor.test rejects unknown method arguments", {
  d <- make_grouped_data()
  expect_error(pairwise.cor.test(d$x, d$g, method = "nope"), "should be one of")
  expect_error(pairwise.cor.test(d$x, d$g, p.adjust.method = "nope"), "should be one of")
})
