# Test make_time function
# Tests for calculating TIME, TAFD, and TAD fields from DTC data

test_that("make_time works with basic single subject data", {
  # Create test data with single subject, single parent compound
  test_data <- tibble::tribble(
    ~ID, ~DTC,                           ~EVID, ~ANALYTE, ~PARENT, ~DV,
    1,   as.POSIXct("2023-01-01 08:00:00"), 1,     "DRUG",   "DRUG",   NA,
    1,   as.POSIXct("2023-01-01 09:00:00"), 0,     "DRUG",   "DRUG",   10,
    1,   as.POSIXct("2023-01-01 10:00:00"), 0,     "DRUG",   "DRUG",   20,
    1,   as.POSIXct("2023-01-01 11:00:00"), 0,     "DRUG",   "DRUG",   15
  ) %>%
    new_nif()

  result <- make_time(test_data)

  # Check that all time fields were added
  expect_true(all(c("TIME", "TAFD", "TAD") %in% names(result)))

  # Check TIME values (hours since first record)
  expect_equal(result$TIME, c(0, 1, 2, 3))

  # Check TAFD values (hours since first administration)
  expect_equal(result$TAFD, c(0, 1, 2, 3))

  # Check TAD values (hours since most recent administration)
  expect_equal(result$TAD, c(0, 1, 2, 3))

  # Check that result is a nif object
  expect_s3_class(result, "nif")
})

test_that("make_time works with multiple subjects", {
  # Create test data with two subjects
  test_data <- tibble::tribble(
    ~ID, ~DTC,                           ~EVID, ~ANALYTE, ~PARENT, ~DV,
    1,   as.POSIXct("2023-01-01 08:00:00"), 1,     "DRUG",   "DRUG",   NA,
    1,   as.POSIXct("2023-01-01 09:00:00"), 0,     "DRUG",   "DRUG",   10,
    1,   as.POSIXct("2023-01-01 10:00:00"), 0,     "DRUG",   "DRUG",   20,
    2,   as.POSIXct("2023-01-01 12:00:00"), 1,     "DRUG",   "DRUG",   NA,
    2,   as.POSIXct("2023-01-01 13:00:00"), 0,     "DRUG",   "DRUG",   30,
    2,   as.POSIXct("2023-01-01 14:00:00"), 0,     "DRUG",   "DRUG",   40
  ) %>%
    new_nif()

  result <- make_time(test_data)

  # Check TIME values (each subject starts at 0)
  expect_equal(result$TIME, c(0, 1, 2, 0, 1, 2))

  # Check TAFD values (each subject starts at 0 for first dose)
  expect_equal(result$TAFD, c(0, 1, 2, 0, 1, 2))

  # Check TAD values (each subject starts at 0 for first dose)
  expect_equal(result$TAD, c(0, 1, 2, 0, 1, 2))
})

test_that("make_time works with multiple parent compounds", {
  # Create test data with two parent compounds for same subject
  test_data <- tibble::tribble(
    ~ID, ~DTC,                           ~EVID, ~ANALYTE, ~PARENT, ~DV,
    1,   as.POSIXct("2023-01-01 08:00:00"), 1,     "DRUG1",  "DRUG1",  NA,
    1,   as.POSIXct("2023-01-01 09:00:00"), 0,     "DRUG1",  "DRUG1",  10,
    1,   as.POSIXct("2023-01-01 10:00:00"), 1,     "DRUG2",  "DRUG2",  NA,
    1,   as.POSIXct("2023-01-01 11:00:00"), 0,     "DRUG2",  "DRUG2",  20,
    1,   as.POSIXct("2023-01-01 12:00:00"), 0,     "DRUG1",  "DRUG1",  15,
    1,   as.POSIXct("2023-01-01 13:00:00"), 0,     "DRUG2",  "DRUG2",  25
  ) %>%
    new_nif()

  result <- make_time(test_data)

  # Check TIME values (all relative to first record at 08:00)
  # Data gets reordered by the function, so we need to check the actual order
  expect_equal(sort(result$TIME), c(0, 1, 2, 3, 4, 5))

  # Check TAFD values (relative to first dose of each parent)
  # DRUG1: first dose at 08:00, so TAFD = 0, 1, 4 for DRUG1 records
  # DRUG2: first dose at 10:00, so TAFD = 0, 1, 3 for DRUG2 records
  expect_equal(sort(result$TAFD), c(0, 0, 1, 1, 3, 4))

  # Check TAD values (relative to most recent dose of each parent)
  # DRUG1: dose at 08:00, so TAD = 0, 1, 4 for DRUG1 records
  # DRUG2: dose at 10:00, so TAD = 0, 1, 3 for DRUG2 records
  expect_equal(sort(result$TAD), c(0, 0, 1, 1, 3, 4))
})

test_that("make_time works with multiple administrations of same parent", {
  # Create test data with multiple doses of same parent compound
  test_data <- tibble::tribble(
    ~ID, ~DTC,                           ~EVID, ~ANALYTE, ~PARENT, ~DV,
    1,   as.POSIXct("2023-01-01 08:00:00"), 1,     "DRUG",   "DRUG",   NA,
    1,   as.POSIXct("2023-01-01 09:00:00"), 0,     "DRUG",   "DRUG",   10,
    1,   as.POSIXct("2023-01-01 10:00:00"), 0,     "DRUG",   "DRUG",   20,
    1,   as.POSIXct("2023-01-01 11:00:00"), 1,     "DRUG",   "DRUG",   NA,  # Second dose
    1,   as.POSIXct("2023-01-01 12:00:00"), 0,     "DRUG",   "DRUG",   15,
    1,   as.POSIXct("2023-01-01 13:00:00"), 0,     "DRUG",   "DRUG",   25
  ) %>%
    new_nif()

  result <- make_time(test_data)

  # Check TIME values (all relative to first record at 08:00)
  expect_equal(result$TIME, c(0, 1, 2, 3, 4, 5))

  # Check TAFD values (all relative to first dose at 08:00)
  expect_equal(result$TAFD, c(0, 1, 2, 3, 4, 5))

  # Check TAD values (relative to most recent dose)
  expect_equal(result$TAD, c(0, 1, 2, 0, 1, 2))
})

test_that("make_time handles observations before first administration", {
  # Create test data with observations before dosing
  test_data <- tibble::tribble(
    ~ID, ~DTC,                           ~EVID, ~ANALYTE, ~PARENT, ~DV,
    1,   as.POSIXct("2023-01-01 07:00:00"), 0,     "DRUG",   "DRUG",   5,   # Pre-dose
    1,   as.POSIXct("2023-01-01 08:00:00"), 0,     "DRUG",   "DRUG",   8,   # Pre-dose
    1,   as.POSIXct("2023-01-01 09:00:00"), 1,     "DRUG",   "DRUG",   NA,  # First dose
    1,   as.POSIXct("2023-01-01 10:00:00"), 0,     "DRUG",   "DRUG",   10,
    1,   as.POSIXct("2023-01-01 11:00:00"), 0,     "DRUG",   "DRUG",   20
  ) %>%
    new_nif()

  result <- make_time(test_data)

  # Check TIME values (all relative to first record at 07:00)
  expect_equal(result$TIME, c(0, 1, 2, 3, 4))

  # Check TAFD values (negative for pre-dose observations)
  expect_equal(result$TAFD, c(-2, -1, 0, 1, 2))

  # Check TAD values (NA for pre-dose observations, then relative to dose)
  expect_equal(result$TAD, c(NA, NA, 0, 1, 2))
})

test_that("make_time handles empty data frame", {
  # Create empty test data
  test_data <- tibble::tribble(
    ~ID, ~DTC, ~EVID, ~ANALYTE, ~PARENT, ~DV
  ) %>%
    new_nif()

  result <- make_time(test_data)

  # Check that time fields are added as empty numeric vectors
  expect_true(all(c("TIME", "TAFD", "TAD") %in% names(result)))
  expect_equal(length(result$TIME), 0)
  expect_equal(length(result$TAFD), 0)
  expect_equal(length(result$TAD), 0)
  expect_s3_class(result, "nif")
})

test_that("make_time validates required columns", {
  # Test missing ID column
  test_data <- tibble::tribble(
    ~DTC,                           ~EVID, ~ANALYTE, ~PARENT, ~DV,
    as.POSIXct("2023-01-01 08:00:00"), 1,     "DRUG",   "DRUG",   NA
  ) %>%
    new_nif()

  expect_error(
    make_time(test_data),
    "Missing required columns: ID"
  )

  # Test missing DTC column
  test_data <- tibble::tribble(
    ~ID, ~EVID, ~ANALYTE, ~PARENT, ~DV,
    1,   1,     "DRUG",   "DRUG",   NA
  ) %>%
    new_nif()

  expect_error(
    make_time(test_data),
    "Missing required columns: DTC"
  )

  # Test missing ANALYTE column
  test_data <- tibble::tribble(
    ~ID, ~DTC,                           ~EVID, ~PARENT, ~DV,
    1,   as.POSIXct("2023-01-01 08:00:00"), 1,     "DRUG",   NA
  ) %>%
    new_nif()

  expect_error(
    make_time(test_data),
    "Missing required columns: ANALYTE"
  )

  # Test missing PARENT column
  test_data <- tibble::tribble(
    ~ID, ~DTC,                           ~EVID, ~ANALYTE, ~DV,
    1,   as.POSIXct("2023-01-01 08:00:00"), 1,     "DRUG",   NA
  ) %>%
    new_nif()

  expect_error(
    make_time(test_data),
    "Missing required columns: PARENT"
  )

  # Test missing EVID column
  test_data <- tibble::tribble(
    ~ID, ~DTC,                           ~ANALYTE, ~PARENT, ~DV,
    1,   as.POSIXct("2023-01-01 08:00:00"), "DRUG",   "DRUG",   NA
  ) %>%
    new_nif()

  expect_error(
    make_time(test_data),
    "Missing required columns: EVID"
  )
})

test_that("make_time validates DTC column type", {
  # Test with character DTC instead of POSIXct
  test_data <- tibble::tribble(
    ~ID, ~DTC,                ~EVID, ~ANALYTE, ~PARENT, ~DV,
    1,   "2023-01-01 08:00:00", 1,     "DRUG",   "DRUG",   NA
  ) %>%
    new_nif()

  expect_error(
    make_time(test_data),
    "DTC column must contain POSIXct datetime values"
  )
})

test_that("make_time validates input is nif object", {
  # Test with regular data frame
  test_data <- data.frame(
    ID = 1,
    DTC = as.POSIXct("2023-01-01 08:00:00"),
    EVID = 1,
    ANALYTE = "DRUG",
    PARENT = "DRUG",
    DV = NA
  )

  expect_error(
    make_time(test_data),
    "Input must be a nif object"
  )
})

test_that("make_time preserves original data columns", {
  # Create test data with extra columns
  test_data <- tibble::tribble(
    ~ID, ~DTC,                           ~EVID, ~ANALYTE, ~PARENT, ~DV, ~EXTRA1, ~EXTRA2,
    1,   as.POSIXct("2023-01-01 08:00:00"), 1,     "DRUG",   "DRUG",   NA,  "A",     123,
    1,   as.POSIXct("2023-01-01 09:00:00"), 0,     "DRUG",   "DRUG",   10,  "B",     456
  ) %>%
    new_nif()

  result <- make_time(test_data)

  # Check that original columns are preserved
  expect_true(all(names(test_data) %in% names(result)))
  expect_equal(result$EXTRA1, test_data$EXTRA1)
  expect_equal(result$EXTRA2, test_data$EXTRA2)
})

test_that("make_time calculates time with correct precision", {
  # Create test data with precise timing
  test_data <- tibble::tribble(
    ~ID, ~DTC,                                    ~EVID, ~ANALYTE, ~PARENT, ~DV,
    1,   as.POSIXct("2023-01-01 08:00:00.000"),     1,     "DRUG",   "DRUG",   NA,
    1,   as.POSIXct("2023-01-01 08:30:00.000"),     0,     "DRUG",   "DRUG",   10,
    1,   as.POSIXct("2023-01-01 09:15:30.000"),     0,     "DRUG",   "DRUG",   20
  ) %>%
    new_nif()

  result <- make_time(test_data)

  # Check TIME values with 3 decimal precision
  expect_equal(result$TIME, c(0, 0.5, 1.258), tolerance = 1e-3)

  # Check TAFD values with 3 decimal precision
  expect_equal(result$TAFD, c(0, 0.5, 1.258), tolerance = 1e-3)

  # Check TAD values with 3 decimal precision
  expect_equal(result$TAD, c(0, 0.5, 1.258), tolerance = 1e-3)
})

test_that("make_time handles subjects with no administrations", {
  # Create test data with subject having only observations (no EVID=1)
  test_data <- tibble::tribble(
    ~ID, ~DTC,                           ~EVID, ~ANALYTE, ~PARENT, ~DV,
    1,   as.POSIXct("2023-01-01 08:00:00"), 0,     "DRUG",   "DRUG",   5,
    1,   as.POSIXct("2023-01-01 09:00:00"), 0,     "DRUG",   "DRUG",   10,
    1,   as.POSIXct("2023-01-01 10:00:00"), 0,     "DRUG",   "DRUG",   15
  ) %>%
    new_nif()

  result <- make_time(test_data)

  # Check TIME values (all relative to first record)
  expect_equal(result$TIME, c(0, 1, 2))

  # Check TAFD values (should be NA since no administrations)
  expect_equal(result$TAFD, c(NA_real_, NA_real_, NA_real_))

  # Check TAD values (should be NA since no administrations)
  expect_equal(result$TAD, c(NA_real_, NA_real_, NA_real_))
})

test_that("make_time handles complex multi-subject, multi-parent scenario", {
  # Create complex test data with multiple subjects and parent compounds
  test_data <- tibble::tribble(
    ~ID, ~DTC,                           ~EVID, ~ANALYTE, ~PARENT, ~DV,
    1,   as.POSIXct("2023-01-01 08:00:00"), 1,     "DRUG1",  "DRUG1",  NA,
    1,   as.POSIXct("2023-01-01 09:00:00"), 0,     "DRUG1",  "DRUG1",  10,
    1,   as.POSIXct("2023-01-01 10:00:00"), 1,     "DRUG2",  "DRUG2",  NA,
    1,   as.POSIXct("2023-01-01 11:00:00"), 0,     "DRUG2",  "DRUG2",  20,
    1,   as.POSIXct("2023-01-01 12:00:00"), 1,     "DRUG1",  "DRUG1",  NA,  # Second dose of DRUG1
    1,   as.POSIXct("2023-01-01 13:00:00"), 0,     "DRUG1",  "DRUG1",  15,
    2,   as.POSIXct("2023-01-01 14:00:00"), 1,     "DRUG1",  "DRUG1",  NA,
    2,   as.POSIXct("2023-01-01 15:00:00"), 0,     "DRUG1",  "DRUG1",  30,
    2,   as.POSIXct("2023-01-01 16:00:00"), 0,     "DRUG1",  "DRUG1",  40
  ) %>%
    new_nif()

  result <- make_time(test_data)

  # Check TIME values (all relative to first record at 08:00)
  # Data gets reordered, so check the sorted values
  expect_equal(sort(result$TIME), c(0, 1, 2, 3, 4, 5, 6, 7, 8))

  # Check TAFD values (relative to first dose of each parent per subject)
  # Subject 1: DRUG1 first at 08:00, DRUG2 first at 10:00
  # Subject 2: DRUG1 first at 14:00
  expect_equal(sort(result$TAFD), c(0, 0, 0, 1, 1, 2, 3, 4, 5))

  # Check TAD values (relative to most recent dose of each parent per subject)
  # Subject 1: DRUG1 doses at 08:00 and 12:00, DRUG2 dose at 10:00
  # Subject 2: DRUG1 dose at 14:00
  expect_equal(sort(result$TAD), c(0, 0, 0, 1, 1, 2, 3, 4, 5))
})

test_that("make_time handles missing DTC values gracefully", {
  # Create test data with some missing DTC values
  test_data <- tibble::tribble(
    ~ID, ~DTC,                           ~EVID, ~ANALYTE, ~PARENT, ~DV,
    1,   as.POSIXct("2023-01-01 08:00:00"), 1,     "DRUG",   "DRUG",   NA,
    1,   as.POSIXct(NA),                     0,     "DRUG",   "DRUG",   10,
    1,   as.POSIXct("2023-01-01 10:00:00"), 0,     "DRUG",   "DRUG",   20
  ) %>%
    new_nif()

  result <- make_time(test_data)

  # Check that function handles missing values
  expect_true(all(c("TIME", "TAFD", "TAD") %in% names(result)))
  expect_s3_class(result, "nif")
  
  # The function should handle missing DTC values by using na.rm=TRUE in min()
  # The exact behavior depends on how the function handles missing values
  expect_equal(length(result$TIME), 3)
})
