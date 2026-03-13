test_that("calc_baseline returns summary of filtered DV values", {
  group_data <- tibble::tribble(
    ~DV,  ~TAFD,
    10,   -1,
    12,    0,
    15,    1,
    18,    2
  )
  filter_expr <- parse(text = "TAFD <= 0")

  result <- calc_baseline(group_data, filter_expr, median, NA_real_)
  expect_equal(result, 11)
})


test_that("calc_baseline works with mean summary function", {
  group_data <- tibble::tribble(
    ~DV,  ~TAFD,
    10,   -1,
    14,    0,
    20,    1
  )
  filter_expr <- parse(text = "TAFD <= 0")

  result <- calc_baseline(group_data, filter_expr, mean, NA_real_)
  expect_equal(result, 12)
})


test_that("calc_baseline works with min summary function", {
  group_data <- tibble::tribble(
    ~DV,  ~TAFD,
    10,   -1,
    14,    0,
    20,    1
  )
  filter_expr <- parse(text = "TAFD <= 0")

  result <- calc_baseline(group_data, filter_expr, min, NA_real_)
  expect_equal(result, 10)
})


test_that("calc_baseline works with max summary function", {
  group_data <- tibble::tribble(
    ~DV,  ~TAFD,
    10,   -1,
    14,    0,
    20,    1
  )
  filter_expr <- parse(text = "TAFD <= 0")

  result <- calc_baseline(group_data, filter_expr, max, NA_real_)
  expect_equal(result, 14)
})


test_that("calc_baseline returns default when filter matches no rows", {
  group_data <- tibble::tribble(
    ~DV,  ~TAFD,
    10,    1,
    12,    2,
    15,    3
  )
  filter_expr <- parse(text = "TAFD <= 0")

  result <- calc_baseline(group_data, filter_expr, median, NA_real_)
  expect_true(is.na(result))
})


test_that("calc_baseline returns custom default when filter matches no rows", {
  group_data <- tibble::tribble(
    ~DV,  ~TAFD,
    10,    1,
    12,    2
  )
  filter_expr <- parse(text = "TAFD <= 0")

  result <- calc_baseline(group_data, filter_expr, median, 999)
  expect_equal(result, 999)
})


test_that("calc_baseline returns default when all filtered DV values are NA", {
  group_data <- tibble::tribble(
    ~DV,       ~TAFD,
    NA_real_,  -1,
    NA_real_,   0,
    15,         1
  )
  filter_expr <- parse(text = "TAFD <= 0")

  result <- calc_baseline(group_data, filter_expr, median, NA_real_)
  expect_true(is.na(result))
})


test_that("calc_baseline returns custom default when all filtered DV are NA", {
  group_data <- tibble::tribble(
    ~DV,       ~TAFD,
    NA_real_,  -1,
    NA_real_,   0,
    15,         1
  )
  filter_expr <- parse(text = "TAFD <= 0")

  result <- calc_baseline(group_data, filter_expr, median, -1)
  expect_equal(result, -1)
})


test_that("calc_baseline removes NA values before summarizing", {
  group_data <- tibble::tribble(
    ~DV,       ~TAFD,
    10,        -1,
    NA_real_,   0,
    15,         1
  )
  filter_expr <- parse(text = "TAFD <= 0")

  result <- calc_baseline(group_data, filter_expr, median, NA_real_)
  expect_equal(result, 10)
})


test_that("calc_baseline handles single matching observation", {
  group_data <- tibble::tribble(
    ~DV,  ~TAFD,
    42,   -1,
    10,    1,
    12,    2
  )
  filter_expr <- parse(text = "TAFD <= -1")

  result <- calc_baseline(group_data, filter_expr, median, NA_real_)
  expect_equal(result, 42)
})


test_that("calc_baseline handles all rows matching the filter", {
  group_data <- tibble::tribble(
    ~DV,  ~TAFD,
    10,   -2,
    12,   -1,
    14,    0
  )
  filter_expr <- parse(text = "TAFD <= 0")

  result <- calc_baseline(group_data, filter_expr, median, NA_real_)
  expect_equal(result, 12)
})


test_that("calc_baseline returns default when summary function returns NaN", {
  nan_fun <- function(x) NaN
  group_data <- tibble::tribble(
    ~DV,  ~TAFD,
    10,   -1,
    12,    0
  )
  filter_expr <- parse(text = "TAFD <= 0")

  result <- calc_baseline(group_data, filter_expr, nan_fun, 77)
  expect_equal(result, 77)
})


test_that("calc_baseline returns default when summary function returns NA", {
  na_fun <- function(x) NA_real_
  group_data <- tibble::tribble(
    ~DV,  ~TAFD,
    10,   -1,
    12,    0
  )
  filter_expr <- parse(text = "TAFD <= 0")

  result <- calc_baseline(group_data, filter_expr, na_fun, 55)
  expect_equal(result, 55)
})


test_that("calc_baseline works with complex filter expressions", {
  group_data <- tibble::tribble(
    ~DV,  ~TAFD, ~EVID,
    10,   -1,    0,
    100,   0,    1,
    12,    0,    0,
    15,    1,    0
  )
  filter_expr <- parse(text = "TAFD <= 0 & EVID == 0")

  result <- calc_baseline(group_data, filter_expr, median, NA_real_)
  expect_equal(result, 11)
})


test_that("calc_baseline works with character column filter", {
  group_data <- tibble::tribble(
    ~DV,  ~FOOD,
    10,   "FED",
    12,   "FED",
    20,   "FASTED"
  )
  filter_expr <- parse(text = "FOOD == 'FED'")

  result <- calc_baseline(group_data, filter_expr, mean, NA_real_)
  expect_equal(result, 11)
})


test_that("calc_baseline handles negative DV values", {
  group_data <- tibble::tribble(
    ~DV,   ~TAFD,
    -10,   -1,
    -20,    0,
    5,      1
  )
  filter_expr <- parse(text = "TAFD <= 0")

  result <- calc_baseline(group_data, filter_expr, median, NA_real_)
  expect_equal(result, -15)
})


test_that("calc_baseline handles zero DV values", {
  group_data <- tibble::tribble(
    ~DV,  ~TAFD,
    0,    -1,
    0,     0,
    5,     1
  )
  filter_expr <- parse(text = "TAFD <= 0")

  result <- calc_baseline(group_data, filter_expr, median, NA_real_)
  expect_equal(result, 0)
})


test_that("calc_baseline with default 0 returns 0 when no data matches", {
  group_data <- tibble::tribble(
    ~DV,  ~TAFD,
    10,    1,
    12,    2
  )
  filter_expr <- parse(text = "TAFD <= 0")

  result <- calc_baseline(group_data, filter_expr, median, 0)
  expect_equal(result, 0)
})


test_that("calc_baseline handles large number of observations", {
  n <- 1000
  group_data <- data.frame(
    DV = seq_len(n),
    TAFD = seq_len(n) - 501
  )
  filter_expr <- parse(text = "TAFD <= 0")

  # TAFD <= 0 matches rows 1:501 (TAFD = -500 to 0), so DV = 1:501
  result <- calc_baseline(group_data, filter_expr, median, NA_real_)
  expect_equal(result, median(1:501))
})


test_that("calc_baseline handles mixed NA and valid DV in baseline window", {
  group_data <- tibble::tribble(
    ~DV,       ~TAFD,
    NA_real_,  -2,
    10,        -1,
    NA_real_,   0,
    12,         0,
    20,         1
  )
  filter_expr <- parse(text = "TAFD <= 0")

  result <- calc_baseline(group_data, filter_expr, mean, NA_real_)
  expect_equal(result, 11)
})
