test_that("geomean calculates the geometric mean of positive values", {
  expect_equal(geomean(c(1, 10, 100)), 10)
  expect_equal(geomean(c(10, 40)), 20)
  expect_equal(geomean(c(2, 8)), 4)
  expect_equal(geomean(c(2, 8, 32)), 8)
  expect_equal(geomean(c(1, 1, 1)), 1)
})


test_that("geomean matches the product formula for positive values", {
  x <- c(1.5, 6, 24, 3)
  expect_equal(geomean(x), prod(x)^(1 / length(x)))
})


test_that("geomean returns the value itself for a single positive number", {
  expect_equal(geomean(5), 5)
  expect_equal(geomean(1L), 1)
  expect_equal(geomean(0.25), 0.25)
})


test_that("geomean accepts integer input", {
  expect_equal(geomean(c(1L, 10L, 100L)), 10)
  expect_equal(geomean(c(4L, 16L)), 8)
})


test_that("geomean returns a length-one numeric scalar", {
  result <- geomean(c(2, 8, 32))
  expect_type(result, "double")
  expect_length(result, 1)
  expect_false(is.nan(result))
})


test_that("geomean yields 0 when any value is zero", {
  expect_equal(geomean(0), 0)
  expect_equal(geomean(c(0, 0)), 0)
  expect_equal(geomean(c(0, 1, 4)), 0)
})


test_that("geomean yields NaN for negative values", {
  expect_warning(
    result <- geomean(c(-1, 4, 16)),
    "NaNs produced"
  )
  expect_true(is.nan(result))
  expect_true(is.nan(suppressWarnings(geomean(-2))))
  expect_true(is.nan(suppressWarnings(geomean(c(-1, -4)))))
})


test_that("geomean returns NA for empty input", {
  expect_identical(geomean(numeric(0)), NA_real_)
  expect_identical(geomean(integer(0)), NA_real_)
})


test_that("geomean returns NA when missing values are present and na.rm is FALSE", {
  expect_identical(geomean(NA_real_), NA_real_)
  expect_identical(geomean(c(1, NA)), NA_real_)
  expect_identical(geomean(c(10, 40, NA_real_)), NA_real_)
  expect_identical(geomean(c(1, NA), na.rm = FALSE), NA_real_)
})


test_that("geomean drops missing values when na.rm is TRUE", {
  expect_equal(geomean(c(1, 10, 100, NA), na.rm = TRUE), 10)
  expect_equal(geomean(c(NA, 4, 16), na.rm = TRUE), 8)
  expect_equal(geomean(c(5, NA_real_), na.rm = TRUE), 5)
})


test_that("geomean returns NA when na.rm leaves no values", {
  expect_identical(geomean(NA_real_, na.rm = TRUE), NA_real_)
  expect_identical(geomean(c(NA_real_, NA_real_), na.rm = TRUE), NA_real_)
})


test_that("geomean treats NaN like missing values", {
  expect_identical(geomean(c(1, NaN)), NA_real_)
  expect_equal(geomean(c(4, 16, NaN), na.rm = TRUE), 8)
})


test_that("geomean yields Inf when an input is infinite", {
  expect_equal(geomean(c(1, Inf)), Inf)
  expect_equal(geomean(Inf), Inf)
})


test_that("geomean errors for non-numeric input", {
  expect_error(geomean("1"), "`x` must be numeric")
  expect_error(geomean(TRUE), "`x` must be numeric")
  expect_error(geomean(factor(c(1, 2))), "`x` must be numeric")
  expect_error(geomean(NULL), "`x` must be numeric")
  expect_error(geomean(list(1, 2)), "`x` must be numeric")
  expect_error(geomean(c(NA, NA)), "`x` must be numeric")
})


test_that("geocv calculates the geometric CV in percent", {
  x <- c(10, 40)
  expected <- sqrt(exp(stats::var(log(x))) - 1) * 100
  expect_equal(geocv(x), expected)
  expect_equal(geocv(x), 127.0457776, tolerance = 1e-6)
})


test_that("geocv is zero when all values are identical", {
  expect_equal(geocv(c(5, 5)), 0)
  expect_equal(geocv(c(2, 2, 2, 2)), 0)
  expect_equal(geocv(c(1L, 1L)), 0)
})


test_that("geocv increases with greater relative spread", {
  expect_true(geocv(c(10, 20)) < geocv(c(10, 40)))
  expect_true(geocv(c(10, 40)) < geocv(c(10, 90)))
})


test_that("geocv accepts integer input", {
  expect_equal(
    geocv(c(10L, 40L)),
    sqrt(exp(stats::var(log(c(10, 40)))) - 1) * 100
  )
})


test_that("geocv returns a length-one numeric scalar", {
  result <- geocv(c(10, 20, 40))
  expect_type(result, "double")
  expect_length(result, 1)
  expect_true(result > 0)
})


test_that("geocv returns NA when fewer than two values are present", {
  expect_identical(geocv(numeric(0)), NA_real_)
  expect_identical(geocv(integer(0)), NA_real_)
  expect_identical(geocv(5), NA_real_)
  expect_identical(geocv(c(10)), NA_real_)
})


test_that("geocv returns NA when missing values are present and na.rm is FALSE", {
  expect_identical(geocv(c(10, 40, NA)), NA_real_)
  expect_identical(geocv(c(NA_real_, NA_real_)), NA_real_)
  expect_identical(geocv(c(10, NA), na.rm = FALSE), NA_real_)
})


test_that("geocv drops missing values when na.rm is TRUE", {
  x <- c(10, 40)
  expected <- sqrt(exp(stats::var(log(x))) - 1) * 100
  expect_equal(geocv(c(10, 40, NA), na.rm = TRUE), expected)
  expect_equal(geocv(c(NA, 10, 40, NA), na.rm = TRUE), expected)
})


test_that("geocv returns NA when na.rm leaves fewer than two values", {
  expect_identical(geocv(NA_real_, na.rm = TRUE), NA_real_)
  expect_identical(geocv(c(NA_real_, NA_real_), na.rm = TRUE), NA_real_)
  expect_identical(geocv(c(10, NA), na.rm = TRUE), NA_real_)
})


test_that("geocv treats NaN like missing values", {
  expect_identical(geocv(c(10, 40, NaN)), NA_real_)
  expect_equal(
    geocv(c(10, 40, NaN), na.rm = TRUE),
    geocv(c(10, 40))
  )
})


test_that("geocv yields NaN for zeros", {
  expect_true(is.nan(geocv(c(0, 1))))
  expect_true(is.nan(geocv(c(0, 0))))
})


test_that("geocv yields NA for negative values", {
  expect_warning(
    result <- geocv(c(-1, 4)),
    "NaNs produced"
  )
  expect_identical(result, NA_real_)
  expect_true(is.na(suppressWarnings(geocv(c(-2, -8)))))
  expect_false(is.nan(suppressWarnings(geocv(c(-2, -8)))))
})


test_that("geocv errors for non-numeric input", {
  expect_error(geocv("1"), "`x` must be numeric")
  expect_error(geocv(TRUE), "`x` must be numeric")
  expect_error(geocv(factor(c(1, 2))), "`x` must be numeric")
  expect_error(geocv(NULL), "`x` must be numeric")
  expect_error(geocv(list(1, 2)), "`x` must be numeric")
  expect_error(geocv(c(NA, NA)), "`x` must be numeric")
})
