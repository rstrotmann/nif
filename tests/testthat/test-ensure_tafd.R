test_that("ensure_tafd works with basic input", {
  # Create a simple test data frame
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~PARENT, ~DV,  ~AMT,
    1,   0,     1,     1,    "DRUG",  NA,   100,
    1,   1,     0,     2,    "DRUG",  10,   0,
    1,   2,     0,     2,    "DRUG",  20,   0,
    2,   0,     1,     1,    "DRUG",  NA,   100,
    2,   1,     0,     2,    "DRUG",  30,   0,
    2,   2,     0,     2,    "DRUG",  40,   0
  ) %>%
    nif()

  result <- ensure_tafd(test_data)

  # Check that TAFD was added
  expect_true("TAFD" %in% names(result))

  # Check TAFD values for first dosing
  expect_equal(result$TAFD, c(0, 1, 2, 0, 1, 2))
})

test_that("ensure_tafd handles multiple administrations", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~PARENT, ~DV,  ~AMT,
    1,   0,     1,     1,    "DRUG",  NA,   100,
    1,   1,     0,     2,    "DRUG",  10,   0,
    1,   2,     1,     1,    "DRUG",  NA,   100,
    1,   3,     0,     2,    "DRUG",  20,   0,
    1,   4,     0,     2,    "DRUG",  30,   0
  ) %>%
    nif()

  result <- ensure_tafd(test_data)

  # Check TAFD values - should be calculated from first dose only
  expect_equal(result$TAFD, c(0, 1, 2, 3, 4))
})

test_that("ensure_tafd handles observations before first dose", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~PARENT, ~DV, ~AMT,
    1,   -1,    0,     2,    "DRUG",  5,   0,
    1,   0,     1,     1,    "DRUG",  NA,  100,
    1,   1,     0,     2,    "DRUG",  10,  0,
    1,   2,     0,     2,    "DRUG",  20,  0
  ) %>%
    nif()

  result <- ensure_tafd(test_data)

  # Check that pre-dose observation has negative TAFD
  expect_equal(result$TAFD, c(-1, 0, 1, 2))
})

test_that("ensure_tafd handles multiple parent compounds", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~PARENT, ~DV,  ~AMT,
    1,   0,     1,     1,    "DRUG1", NA,   100,
    1,   1,     0,     2,    "DRUG1", 10,   0,
    1,   2,     0,     2,    "DRUG1", 20,   0,
    1,   0,     1,     3,    "DRUG2", NA,   100,
    1,   1,     0,     4,    "DRUG2", 30,   0,
    1,   2,     0,     4,    "DRUG2", 40,   0
  ) %>%
    nif()

  result <- ensure_tafd(test_data)

  # Check TAFD values for each parent
  expect_equal(result$TAFD, c(0, 0, 1, 1, 2, 2))
})

test_that("ensure_tafd handles missing required columns", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~CMT, ~AMT, ~EVID, ~DV, ~PARENT,
    1,   0,     1,    100,  1,     NA,  "DRUG",
    1,   1,     2,    0,    0,     10,  "DRUG"
  ) |>
    nif() |>
    select(-TIME)

  expect_error(ensure_tafd(test_data), "Missing required columns for TAFD calculation")
})

test_that("ensure_tafd preserves original data", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~PARENT, ~DV,  ~EXTRA, ~AMT,
    1,   0,     1,     1,    "DRUG",  NA,   "A",    100,
    1,   1,     0,     2,    "DRUG",  10,   "B",    0,
    1,   2,     0,     2,    "DRUG",  20,   "C",    0
  ) %>%
    nif()

  result <- ensure_tafd(test_data)

  # Check that original columns are preserved
  expect_true(all(names(test_data) %in% names(result)))
  expect_equal(result$EXTRA, test_data$EXTRA)
})

test_that("ensure_tafd handles NA values in TIME", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~PARENT, ~DV,  ~AMT,
    1,   0,     1,     1,    "DRUG",  NA,   100,
    1,   1,     0,     2,    "DRUG",  10,   0,
    1,   NA,    0,     2,    "DRUG",  20,   0
  ) %>%
    nif()

  expect_no_error(result <- ensure_tafd(test_data))
  expect_equal(result$TIME, result$TAFD)
})

test_that("ensure_tafd returns a nif object", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~PARENT, ~DV,  ~AMT,
    1,   0,     1,     1,    "DRUG",  NA,   100,
    1,   1,     0,     2,    "DRUG",  10,   0
  ) %>%
    nif()

  result <- ensure_tafd(test_data)

  expect_s3_class(result, "nif")
})

test_that("ensure_tafd handles non-nif input", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~PARENT, ~DV,  ~AMT,
    1,   0,     1,     1,    "DRUG",  NA,   100
  )

  expect_error(ensure_tafd(test_data), "Input must be a NIF object")
})

test_that("ensure_tafd handles existing TAFD column", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~PARENT, ~DV,  ~TAFD, ~AMT,
    1,   0,     1,     1,    "DRUG",  NA,   0,     100,
    1,   1,     0,     2,    "DRUG",  10,   1,     0
  ) %>%
    nif()

  result <- ensure_tafd(test_data)

  # Check that original TAFD values are preserved
  expect_equal(result$TAFD, c(0, 1))
})


test_that("ensure_tafd handles NA values in ID", {
  test_data <- tibble::tribble(
    ~ID,  ~TIME, ~EVID, ~CMT, ~PARENT, ~DV,  ~AMT,
    1,    0,     1,     1,    "DRUG",  NA,   100,
    1,   1,     0,     2,    "DRUG",  10,   0
  ) %>%
    nif()

  test_data[2, "ID"] <- NA

  expect_error(ensure_tafd(test_data), "ID colum must not contain NA values!")
})

test_that("ensure_tafd handles no dosing events", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~PARENT, ~DV, ~AMT,
    1,   0,     0,     1,    "DRUG",  5,   0,
    1,   1,     0,     2,    "DRUG",  10,  0,
    1,   2,     0,     2,    "DRUG",  20,  0
  ) %>%
    nif()

  expect_error(
    ensure_tafd(test_data),
    "No dosing event, TAFD cannot be calculated"
  )
})

test_that("ensure_tafd handles different first dose times", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~PARENT, ~DV,  ~AMT,
    1,   0,     1,     1,    "DRUG",  NA,   100,
    1,   1,     0,     2,    "DRUG",  10,   0,
    1,   2,     0,     2,    "DRUG",  20,   0,
    2,   2,     1,     1,    "DRUG",  NA,   100,
    2,   3,     0,     2,    "DRUG",  30,   0,
    2,   4,     0,     2,    "DRUG",  40,   0
  ) %>%
    nif()

  result <- ensure_tafd(test_data)

  # Check TAFD values relative to each subject's first dose
  expect_equal(result$TAFD, c(0, 1, 2, 0, 1, 2))
})

