# Standard skip for tests documenting a confirmed defect in the package
# source. The test body asserts the *correct* behaviour so it can be enabled
# once the defect is fixed upstream. See tests/testthat/KNOWN_BUGS.md.
skip_known_bug <- function(id) {
  skip(paste0("Known bug ", id, " (see tests/testthat/KNOWN_BUGS.md)"))
}

