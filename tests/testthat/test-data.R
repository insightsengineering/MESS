# Every dataset shipped with MESS loads with the expected class, dimensions
# and variable names.
datasets <- list(
  bdstat      = list(dim = c(1356, 4), names = c("year", "month", "births", "dead")),
  bees        = list(dim = c(72, 7), names = c("Locality", "Replicate", "Color", "Time", "Type", "Number", "id")),
  clotting    = list(dim = c(158, 6), names = c("rat", "locality", "sex", "weight", "PCA0", "PCA4")),
  earthquakes = list(dim = c(19777, 7), names = c("time", "latitude", "longitude", "depth", "mag", "place", "type")),
  greenland   = list(dim = c(51, 2), names = c("year", "airtemp")),
  happiness   = list(dim = c(148, 6), names = c("country", "happy", "tax", "religion", "continent", "population")),
  icecreamads = list(dim = c(30, 4), names = c("Price", "Temperature", "Consumption", "Advertise")),
  kwdata      = list(dim = c(18, 3), names = c("x", "y", "z")),
  lifeexpect  = list(dim = c(70, 4), names = c("year", "male", "female", "myear")),
  matched     = list(dim = c(450, 4), names = c("id", "iscase", "vaccine", "lung")),
  nh4         = list(dim = c(120, 3), names = c("nh4", "cens", "year")),
  picea       = list(dim = c(160, 5), names = c("date", "temp", "conc", "damage", "count")),
  qpcr        = list(dim = c(630, 4), names = c("flour", "cycle", "line", "transcript")),
  rainman     = list(dim = c(30, 6), names = c("SAND", "ME", "TM", "AJ", "BM", "LO")),
  soccer      = list(dim = c(805, 5), names = c("name", "DoB", "position", "matches", "goals")),
  superroot2  = list(dim = c(258000, 5), names = c("color", "array", "gene", "plant", "signal"))
)

load_mess_data <- function(name) {
  env <- new.env()
  utils::data(list = name, package = "MESS", envir = env)
  env[[name]]
}

test_that("the expected set of datasets is shipped", {
  shipped <- utils::data(package = "MESS")$results[, "Item"]
  expect_setequal(shipped, c(names(datasets), "smokehealth"))
})

test_that("each data frame has the documented dimensions and variables", {
  for (name in names(datasets)) {
    x <- load_mess_data(name)
    expect_s3_class(x, "data.frame")
    expect_equal(dim(x), datasets[[name]]$dim, label = paste("dim of", name))
    expect_named(x, datasets[[name]]$names)
  }
})

test_that("smokehealth is a 5 x 4 contingency table", {
  x <- load_mess_data("smokehealth")
  expect_s3_class(x, "table")
  expect_equal(dim(x), c(5, 4))
})
