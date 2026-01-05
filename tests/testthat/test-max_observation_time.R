test_that("max_observation_time returns max time for observations only", {
  nif <- tibble::tribble(
  ~ID, ~TIME, ~EVID, ~DV, ~CMT, ~AMT,
    1,     0,     1,  NA,    1,   10,
    1,     1,     0,  10,    2,   10,
    1,     5,     0,  20,    2,   10,
    1,    10,     0,  15,    2,   10,
    1,    12,     1,  NA,    1,   10
  ) |>
    nif()

  expect_equal(max_observation_time(nif), 10)
})

test_that("max_observation_time ignores dosing events (EVID == 1)", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV, ~CMT, ~AMT,
    1,     0,     1,  NA,    1,   10,
    1,     1,     0,  10,    2,   10,
    1,    50,     1,  NA,    1,   10,
    1,     5,     0,  20,    2,   10
  ) %>%
    nif()

  expect_equal(max_observation_time(nif), 5)
})

test_that("max_observation_time filters by analyte when specified", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV, ~CMT, ~ANALYTE, ~AMT,
    1,     1,     0,  10,    2,   "CMT2",   10,
    1,     5,     0,  20,    2,   "CMT2",   10,
    1,    10,     0,  15,    3,   "CMT3",   10,
    1,    20,     0,  25,    3,   "CMT3",   10
  ) %>%
    nif()

  expect_equal(max_observation_time(nif, analyte = "CMT2"), 5)
  expect_equal(max_observation_time(nif, analyte = "CMT3"), 20)
})

test_that("max_observation_time returns max across all analytes when analyte is NULL", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV, ~CMT, ~ANALYTE, ~AMT,
    1,     1,     0,  10,    2,   "CMT2",   10,
    1,     5,     0,  20,    2,   "CMT2",   10,
    1,    10,     0,  15,    3,   "CMT3",   10,
    1,    20,     0,  25,    3,   "CMT3",   10
  ) %>%
    nif()

  expect_equal(max_observation_time(nif, analyte = NULL), 20)
  expect_equal(max_observation_time(nif), 20)  # NULL is default
})

test_that("max_observation_time returns NA when no observations exist", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV, ~CMT, ~AMT,
    1,     0,     1,  NA,    1,   10,
    1,    24,     1,  NA,    1,   10,
    1,    48,     1,  NA,    1,   10
  ) %>%
    nif()

  expect_equal(max_observation_time(nif), NA)
})

test_that("max_observation_time returns NA when analyte has no observations", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV, ~CMT, ~ANALYTE, ~AMT,
    1,   1,     0,     10,  2,    "CMT2",   10,
    1,   5,     0,     20,  2,    "CMT2",   10
  ) %>%
    nif()

  expect_equal(max_observation_time(nif, analyte = "CMT4"), NA)
  expect_equal(max_observation_time(nif, analyte = "NONEXISTENT"), NA)
})

test_that("max_observation_time handles empty nif object", {
  nif <- nif()

  expect_equal(max_observation_time(nif), NA)
})

test_that("max_observation_time handles single observation", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV, ~CMT, ~AMT,
    1,   5,     0,     10,  2,    10
  ) %>%
    nif()

  expect_equal(max_observation_time(nif), 5)
})

test_that("max_observation_time handles NA values in TIME", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV, ~CMT, ~AMT,
    1,   1,     0,     10,  2,    10,
    1,   NA,    0,     20,  2,    10,    # NA time should be ignored
    1,   5,     0,     15,  2,    10
  ) %>%
    nif()

  expect_equal(max_observation_time(nif), 5)
})

test_that("max_observation_time creates ANALYTE from CMT when missing", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV, ~CMT, ~AMT,
    1,   1,     0,     10,  2,    10,
    1,   5,     0,     20,  2,    10,
    1,   10,    0,     15,  3,    10
  ) %>%
    nif()

  # Should work even without ANALYTE column (ensure_analyte creates it)
  result <- max_observation_time(nif)
  expect_equal(result, 10)
  expect_true("ANALYTE" %in% names(ensure_analyte(nif)))
})

test_that("max_observation_time works with multiple subjects", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV, ~CMT, ~AMT,
    1,   1,     0,     10,  2,    10,
    1,   5,     0,     20,  2,    10,
    2,   2,     0,     15,  2,    10,
    2,   8,     0,     25,  2,    10,
    3,   3,     0,     12,  2,    10,
    3,   12,    0,     30,  2,    10
  ) %>%
    nif()

  expect_equal(max_observation_time(nif), 12)
})

test_that("max_observation_time handles zero time observations", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV, ~CMT, ~AMT,
    1,   0,     0,     0,   2,    10,    # zero time observation
    1,   1,     0,     10,  2,    10,
    1,   5,     0,     20,  2,    10
  ) %>%
    nif()

  expect_equal(max_observation_time(nif), 5)
})

test_that("max_observation_time handles negative time observations", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV, ~CMT, ~AMT,
    1,   -1,    0,     5,   2,    10,    # negative time observation
    1,   0,     0,     0,   2,    10,
    1,   5,     0,     20,  2,    10
  ) %>%
    nif()

  expect_equal(max_observation_time(nif), 5)
})

test_that("max_observation_time works with multiple analytes and filters correctly", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV, ~CMT, ~ANALYTE, ~AMT,
    1,   1,     0,     10,  2,    "DRUG",   10,
    1,   5,     0,     20,  2,    "DRUG",   10,
    1,   10,    0,     15,  3,    "METABOLITE",   10,
    1,   20,    0,     25,  3,    "METABOLITE",   10,
    1,   30,    0,     30,  4,    "METABOLITE2",   10
  ) %>%
    nif()

  expect_equal(max_observation_time(nif, analyte = "DRUG"), 5)
  expect_equal(max_observation_time(nif, analyte = "METABOLITE"), 20)
  expect_equal(max_observation_time(nif, analyte = "METABOLITE2"), 30)
  expect_equal(max_observation_time(nif), 30)  # max across all
})

test_that("max_observation_time handles vector of analytes", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV, ~CMT, ~ANALYTE, ~AMT,
    1,   1,     0,     10,  2,    "CMT2",   10,
    1,   5,     0,     20,  2,    "CMT2",   10,
    1,   10,    0,     15,  3,    "CMT3",   10,
    1,   20,    0,     25,  3,    "CMT3",   10,
    1,   30,    0,     30,  4,    "CMT4",   10
  ) %>%
    nif()

  # When multiple analytes specified, should return max across those analytes
  result <- max_observation_time(nif, analyte = c("CMT2", "CMT3"))
  expect_equal(result, 20)
})

test_that("max_observation_time handles all NA times in observations", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV, ~CMT, ~AMT,
    1,   NA,    0,     10,  2,    10,
    1,   NA,    0,     20,  2,    10
  ) %>%
    nif()

  # When all times are NA, max with na.rm=TRUE returns -Inf
  result <- max_observation_time(nif)
  expect_true(is.na(result))
})

test_that("max_observation_time works with decimal time values", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV, ~CMT, ~AMT,
    1,   0.5,   0,     10,  2,    10,
    1,   1.25,  0,     20,  2,    10,
    1,   2.75,  0,     15,  2,    10,
    1,   10.5,  0,     25,  2,    10
  ) %>%
    nif()

  expect_equal(max_observation_time(nif), 10.5)
})

test_that("max_observation_time handles very large time values", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV, ~CMT, ~AMT,
    1,   1,     0,     10,  2,    10,
    1,   1000,  0,     20,  2,    10,
    1,   500,   0,     15,  2,    10
  ) %>%
    nif()

  expect_equal(max_observation_time(nif), 1000)
})

test_that("max_observation_time correctly filters when analyte column exists", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV, ~CMT, ~ANALYTE, ~AMT,
    1,   1,     0,     10,  2,    "ANALYTE1",   10,
    1,   5,     0,     20,  2,    "ANALYTE1",   10,
    1,   10,    0,     15,  2,    "ANALYTE2",   10
  ) %>%
    nif()

  expect_equal(max_observation_time(nif, analyte = "ANALYTE1"), 5)
  expect_equal(max_observation_time(nif, analyte = "ANALYTE2"), 10)
})

test_that("max_observation_time returns correct value when all observations have same time", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~DV, ~CMT, ~AMT,
    1,   5,     0,     10,  2,    10,
    1,   5,     0,     20,  2,    10,
    1,   5,     0,     15,  2,    10
  ) %>%
    nif()

  expect_equal(max_observation_time(nif), 5)
})

