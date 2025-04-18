test_that("ensure_time works with TIME, TAD, and TAFD already present", {
  # Create test data with TIME, TAD, and TAFD already present
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~TAFD, ~TAD, ~EVID, ~DV,
    1,   0,     0,     0,    1,     NA,
    1,   1,     1,     1,    0,     10,
    1,   2,     2,     2,    0,     20,
    2,   0,     0,     0,    1,     NA,
    2,   1,     1,     1,    0,     30
  ) %>%
    new_nif()

  result <- ensure_time(test_data)

  # Should return the original data without changes
  expect_identical(result, test_data)
})

test_that("ensure_time calculates TIME, TAD, and TAFD from DTC", {
  # Create test data with DTC but no TIME, TAD, or TAFD
  test_data <- tibble::tribble(
    ~ID, ~DTC,                  ~EVID, ~ANALYTE, ~PARENT, ~DV,
    1,   as.POSIXct("2023-01-01 08:00:00"), 1,     "DRUG",   "DRUG",   NA,
    1,   as.POSIXct("2023-01-01 09:00:00"), 0,     "DRUG",   "DRUG",   10,
    1,   as.POSIXct("2023-01-01 10:00:00"), 0,     "DRUG",   "DRUG",   20,
    2,   as.POSIXct("2023-01-01 08:00:00"), 1,     "DRUG",   "DRUG",   NA,
    2,   as.POSIXct("2023-01-01 09:00:00"), 0,     "DRUG",   "DRUG",   30
  ) %>%
    new_nif()

  result <- ensure_time(test_data)

  # Check that TIME, TAD, and TAFD were added
  expect_true(all(c("TIME", "TAD", "TAFD") %in% names(result)))

  # Check TIME values (hours since first record)
  expect_equal(result$TIME, c(0, 1, 2, 0, 1))

  # Check TAFD values (hours since first dose)
  expect_equal(result$TAFD, c(0, 1, 2, 0, 1))

  # Check TAD values (hours since most recent dose)
  expect_equal(result$TAD, c(0, 1, 2, 0, 1))
})

test_that("ensure_time calculates TIME, TAD, and TAFD from TIME", {
  # Create test data with TIME but no TAD or TAFD
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~DV,
    1,   0,     1,     1,    NA,
    1,   1,     0,     2,    10,
    1,   2,     0,     2,    20,
    2,   0,     1,     1,    NA,
    2,   1,     0,     2,    30
  ) %>%
    new_nif()

  result <- ensure_time(test_data)

  # Check that TAD and TAFD were added
  expect_true(all(c("TAD", "TAFD") %in% names(result)))

  # Check TAFD values (should be same as TIME since first dose is at TIME=0)
  expect_equal(result$TAFD, c(0, 1, 2, 0, 1))

  # Check TAD values (should be same as TIME for this simple case)
  expect_equal(result$TAD, c(0, 1, 2, 0, 1))
})

test_that("ensure_time handles multiple dosing events", {
  # Create test data with multiple doses
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~DV,
    1,   0,     1,     1,    NA,
    1,   1,     0,     2,    10,
    1,   2,     1,     1,    NA,  # Second dose
    1,   3,     0,     2,    20,
    1,   4,     0,     2,    15
  ) %>%
    new_nif()

  result <- ensure_time(test_data)

  # Check that TAD and TAFD were added
  expect_true(all(c("TAD", "TAFD") %in% names(result)))

  # Check TAFD values (hours since first dose)
  expect_equal(result$TAFD, c(0, 1, 2, 3, 4))

  # Check TAD values (hours since most recent dose)
  expect_equal(result$TAD, c(0, 1, 0, 1, 2))
})

test_that("ensure_time handles multiple parent compounds", {
  # Create test data with multiple parent compounds
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~PARENT, ~DV,
      1,   0,     1,     1,    "DRUG1", NA,
      1,   1,     0,     2,    "DRUG1", 10,
      1,   2,     0,     2,    "DRUG1", 20,
      1,   3,     1,     3,    "DRUG2", NA,  # Different parent compound
      1,   4,     0,     4,    "DRUG2", 30,
      1,   5,     0,     4,    "DRUG2", 25
  ) %>%
    new_nif()

  result <- ensure_time(test_data)

  # Check that TAD and TAFD were added
  expect_true(all(c("TAD", "TAFD") %in% names(result)))

  # Check TAFD values (hours since first dose of respective parent)
  expect_equal(result$TAFD, c(0, 1, 2, 0, 1, 2))

  # Check TAD values (hours since most recent dose of respective parent)
  expect_equal(result$TAD, c(0, 1, 2, 0, 1, 2))
})

test_that("ensure_time handles observations before first dose", {
  # Create test data with observations before dosing
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~DV,
    1,   -2,    0,     2,    5,   # Pre-dose observation
    1,   -1,    0,     2,    8,   # Pre-dose observation
    1,   0,     1,     1,    NA,  # Dose
    1,   1,     0,     2,    10,
    1,   2,     0,     2,    20
  ) %>%
    new_nif()

  result <- ensure_time(test_data)

  # Check that TAD and TAFD were added
  expect_true(all(c("TAD", "TAFD") %in% names(result)))

  # Check TAFD values (negative for pre-dose observations)
  expect_equal(result$TAFD, c(-2, -1, 0, 1, 2))

  # Check TAD values (should be NA for pre-dose observations)
  # We expect NA for pre-dose TAD values based on the add_tad implementation
  expect_equal(result$TAD, c(NA, NA, 0, 1, 2))
})

test_that("ensure_time handles missing required columns", {
  # Create test data without required columns
  test_data <- tibble::tribble(
    ~ID, ~DV,
    1,   10,
    1,   20
  ) %>%
    new_nif()

  expect_error(
    result <- ensure_time(test_data),
    "Either DTC or TIME is required to calculate time fields")
})

test_that("ensure_time preserves original data", {
  # Create test data with extra columns
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~DV, ~EXTRA1, ~EXTRA2,
    1,   0,     1,     1,    NA,  "A",     123,
    1,   1,     0,     2,    10,  "B",     456,
    1,   2,     0,     2,    20,  "C",     789
  ) %>%
    new_nif()

  result <- ensure_time(test_data)

  # Check that original columns are preserved
  expect_true(all(names(test_data) %in% names(result)))
  expect_equal(result$EXTRA1, test_data$EXTRA1)
  expect_equal(result$EXTRA2, test_data$EXTRA2)
})

test_that("ensure_time returns a nif object", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~DV,
    1,   0,     1,     1,    NA,
    1,   1,     0,     2,    10
  ) %>%
    new_nif()

  result <- ensure_time(test_data)

  expect_s3_class(result, "nif")
})

