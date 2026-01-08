test_that("make_time_from_TIME works with basic single subject data", {
  # Create test data with single subject, single parent compound
  # TIME is already present (not calculated from DTC)
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~ANALYTE, ~PARENT, ~DV,  ~AMT, ~CMT,
    1,   0,     1,     "DRUG",   "DRUG",  NA,  100,  1,
    1,   1,     0,     "DRUG",   "DRUG",  10,  0,    2,
    1,   2,     0,     "DRUG",   "DRUG",  20,  0,    2,
    1,   3,     0,     "DRUG",   "DRUG",  15,  0,    2
  ) %>%
    nif()

  result <- make_time_from_TIME(test_data)

  # Check that TAFD and TAD fields were added
  expect_true(all(c("TAFD", "TAD") %in% names(result)))

  # Check that TIME is preserved
  expect_equal(result$TIME, c(0, 1, 2, 3))

  # Check TAFD values (hours since first administration)
  expect_equal(result$TAFD, c(0, 1, 2, 3))

  # Check TAD values (hours since most recent administration)
  expect_equal(result$TAD, c(0, 1, 2, 3))

  # Check that result is a nif object
  expect_s3_class(result, "nif")
})


test_that("make_time_from_TIME works with multiple subjects", {
  # Create test data with two subjects
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~ANALYTE, ~PARENT, ~DV,  ~AMT, ~CMT,
    1,   0,     1,     "DRUG",   "DRUG",  NA,  100,  1,
    1,   1,     0,     "DRUG",   "DRUG",  10,  0,    2,
    1,   2,     0,     "DRUG",   "DRUG",  20,  0,    2,
    2,   0,     1,     "DRUG",   "DRUG",  NA,  100,  1,
    2,   1,     0,     "DRUG",   "DRUG",  30,  0,    2,
    2,   2,     0,     "DRUG",   "DRUG",  40,  0,    2
  ) %>%
    nif()

  result <- make_time_from_TIME(test_data)

  # Check TIME values (preserved as-is)
  expect_equal(result$TIME, c(0, 1, 2, 0, 1, 2))

  # Check TAFD values (each subject starts at 0 for first dose)
  expect_equal(result$TAFD, c(0, 1, 2, 0, 1, 2))

  # Check TAD values (each subject starts at 0 for first dose)
  expect_equal(result$TAD, c(0, 1, 2, 0, 1, 2))
})


test_that("make_time_from_TIME works with multiple parent compounds", {
  # Create test data with two parent compounds for same subject
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~ANALYTE, ~PARENT, ~DV,  ~AMT, ~CMT,
    1,   0,     1,     "DRUG1",  "DRUG1", NA,  100,  1,
    1,   1,     0,     "DRUG1",  "DRUG1", 10,  0,    2,
    1,   2,     1,     "DRUG2",  "DRUG2", NA,  100,  3,
    1,   3,     0,     "DRUG2",  "DRUG2", 20,  0,    4,
    1,   4,     0,     "DRUG1",  "DRUG1", 15,  0,    2,
    1,   5,     0,     "DRUG2",  "DRUG2", 25,  0,    4
  ) %>%
    nif()

  result <- make_time_from_TIME(test_data) |>
    arrange(ID, PARENT)

  # Check TIME values (preserved as-is)
  # Data gets reordered by the function
  expect_equal(result$TIME, c(0, 1, 4, 2, 3, 5))

  # Check TAFD values (relative to first dose of each parent)
  # DRUG1: first dose at TIME=0, so TAFD = 0, 1, 4 for DRUG1 records
  # DRUG2: first dose at TIME=2, so TAFD = 0, 1, 3 for DRUG2 records
  expect_equal(result$TAFD, c(0, 1, 4, 0, 1, 3))

  # Check TAD values (relative to most recent dose of each parent)
  # DRUG1: dose at TIME=0, so TAD = 0, 1, 4 for DRUG1 records
  # DRUG2: dose at TIME=2, so TAD = 0, 1, 3 for DRUG2 records
  expect_equal(result$TAD, c(0, 1, 4, 0, 1, 3))
})


test_that("make_time_from_TIME works with multiple administrations of same parent", {
  # Create test data with multiple doses of same parent compound
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~ANALYTE, ~PARENT, ~DV,  ~AMT, ~CMT,
    1,   0,     1,     "DRUG",   "DRUG",  NA,  100,  1,
    1,   1,     0,     "DRUG",   "DRUG",  10,  0,    2,
    1,   2,     0,     "DRUG",   "DRUG",  20,  0,    2,
    1,   3,     1,     "DRUG",   "DRUG",  NA,  100,  1,    # Second dose
    1,   4,     0,     "DRUG",   "DRUG",  15,  0,    2,
    1,   5,     0,     "DRUG",   "DRUG",  25,  0,    2
  ) %>%
    nif()

  result <- make_time_from_TIME(test_data)

  # Check TIME values (preserved as-is)
  expect_equal(result$TIME, c(0, 1, 2, 3, 4, 5))

  # Check TAFD values (all relative to first dose at TIME=0)
  expect_equal(result$TAFD, c(0, 1, 2, 3, 4, 5))

  # Check TAD values (relative to most recent dose)
  # First dose at TIME=0, second dose at TIME=3
  expect_equal(result$TAD, c(0, 1, 2, 0, 1, 2))
})


test_that("make_time_from_TIME handles observations before first administration", {
  # Create test data with observations before dosing
  # With downup fill, pre-dose observations should have negative TAD
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~ANALYTE, ~PARENT, ~DV, ~AMT, ~CMT,
    1,   -1,    0,     "DRUG",   "DRUG",  5,  0,    2,   # Pre-dose
    1,   0,     0,     "DRUG",   "DRUG",  8,  0,    2,   # Pre-dose
    1,   1,     1,     "DRUG",   "DRUG",  NA, 100,  1,   # First dose
    1,   2,     0,     "DRUG",   "DRUG",  10, 0,    2,
    1,   3,     0,     "DRUG",   "DRUG",  20, 0,    2
  ) %>%
    nif()

  result <- make_time_from_TIME(test_data)

  # Check TIME values (preserved as-is)
  expect_equal(result$TIME, c(-1, 0, 1, 2, 3))

  # Check TAFD values (negative for pre-dose observations)
  expect_equal(result$TAFD, c(-2, -1, 0, 1, 2))

  # Check TAD values (negative for pre-dose observations with downup fill)
  # The downup fill should fill backward, so pre-dose obs get negative TAD
  expect_equal(result$TAD, c(-2, -1, 0, 1, 2))
})


test_that("make_time_from_TIME handles empty data frame", {
  # Create empty test data
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~ANALYTE, ~PARENT, ~DV, ~AMT, ~CMT
  ) %>%
    nif()

  result <- make_time_from_TIME(test_data)

  # Check that TAFD and TAD fields are added as empty numeric vectors
  expect_true(all(c("TAFD", "TAD") %in% names(result)))
  expect_equal(length(result$TAFD), 0)
  expect_equal(length(result$TAD), 0)
  expect_s3_class(result, "nif")
})


test_that("make_time_from_TIME validates input is nif object", {
  # Test with regular data frame
  test_data <- data.frame(
    ID = 1,
    TIME = 0,
    EVID = 1,
    ANALYTE = "DRUG",
    PARENT = "DRUG",
    DV = NA
  )

  expect_error(
    make_time_from_TIME(test_data),
    "Input must be a nif object"
  )
})


test_that("make_time_from_TIME preserves original data columns", {
  # Create test data with extra columns
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~ANALYTE, ~PARENT, ~DV,  ~EXTRA1, ~EXTRA2, ~AMT, ~CMT,
    1,   0,     1,     "DRUG",   "DRUG",  NA,   "A",     123,     100,  1,
    1,   1,     0,     "DRUG",   "DRUG",  10,   "B",     456,     0,    2
  ) %>%
    nif()

  result <- make_time_from_TIME(test_data)

  # Check that original columns are preserved
  expect_true(all(names(test_data) %in% names(result)))
  expect_equal(result$EXTRA1, test_data$EXTRA1)
  expect_equal(result$EXTRA2, test_data$EXTRA2)
})


test_that("make_time_from_TIME calculates time with correct precision", {
  # Create test data with precise timing
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~ANALYTE, ~PARENT, ~DV,  ~AMT, ~CMT,
    1,   0,     1,     "DRUG",   "DRUG",  NA,  100,  1,
    1,   0.5,   0,     "DRUG",   "DRUG",  10,  0,    2,
    1,   1.258, 0,     "DRUG",   "DRUG",  20,  0,    2
  ) %>%
    nif()

  result <- make_time_from_TIME(test_data)

  # Check TIME values (preserved as-is)
  expect_equal(result$TIME, c(0, 0.5, 1.258), tolerance = 1e-3)

  # Check TAFD values with 3 decimal precision
  expect_equal(result$TAFD, c(0, 0.5, 1.258), tolerance = 1e-3)

  # Check TAD values with 3 decimal precision
  expect_equal(result$TAD, c(0, 0.5, 1.258), tolerance = 1e-3)
})


test_that("make_time_from_TIME handles subjects with no administrations", {
  # Create test data with subject having only observations (no EVID=1)
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~ANALYTE, ~PARENT, ~DV, ~AMT, ~CMT,
    1,   0,     0,     "DRUG",   "DRUG",  5,  0,    2,
    1,   1,     0,     "DRUG",   "DRUG",  10, 0,    2,
    1,   2,     0,     "DRUG",   "DRUG",  15, 0,    2
  ) %>%
    nif()

  expect_no_error(
    result <- make_time_from_TIME(test_data)
  )

  expect_equal(result$TAFD, rep(NA_real_, 3))
  expect_equal(result$TAD, rep(NA_real_, 3))
})


test_that("make_time_from_TIME handles complex multi-subject, multi-parent scenario", {
  # Create complex test data with multiple subjects and parent compounds
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~ANALYTE, ~PARENT, ~DV,  ~AMT, ~CMT,
    1,   0,     1,     "DRUG1",  "DRUG1", NA,  100,  1,
    1,   1,     0,     "DRUG1",  "DRUG1", 10,  0,    2,
    1,   2,     1,     "DRUG2",  "DRUG2", NA,  100,  3,
    1,   3,     0,     "DRUG2",  "DRUG2", 20,  0,    4,
    1,   4,     1,     "DRUG1",  "DRUG1", NA,  100,  1,   # Second dose of DRUG1
    1,   5,     0,     "DRUG1",  "DRUG1", 15,  0,    2,
    2,   0,     1,     "DRUG1",  "DRUG1", NA,  100,  1,
    2,   1,     0,     "DRUG1",  "DRUG1", 30,  0,    2,
    2,   2,     0,     "DRUG1",  "DRUG1", 40,  0,    2
  ) %>%
    nif()

  result <- make_time_from_TIME(test_data)

  # Check TIME values (preserved as-is, but reordered by function)
  expect_equal(result$TIME, c(0, 1, 2, 3, 4, 5, 0, 1, 2))

  # Check TAFD values (relative to first dose of each parent per subject)
  # Subject 1: DRUG1 first at TIME=0, DRUG2 first at TIME=2
  # Subject 2: DRUG1 first at TIME=0
  expect_equal(result$TAFD, c(0, 1, 0, 1, 4, 5, 0, 1, 2))

  # Check TAD values (relative to most recent dose of each parent per subject)
  # Subject 1: DRUG1 doses at TIME=0 and 4, DRUG2 dose at TIME=2
  # Subject 2: DRUG1 dose at TIME=0
  expect_equal(result$TAD, c(0, 1, 0, 1, 0, 1, 0, 1, 2))
})


test_that("make_time_from_TIME handles missing TIME values gracefully", {
  # Create test data with some missing TIME values
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~ANALYTE, ~PARENT, ~DV,  ~AMT, ~CMT,
    1,   0,     1,     "DRUG",   "DRUG",  NA,  100,  1,
    1,   NA,    0,     "DRUG",   "DRUG",  10,  0,    2,
    1,   2,     0,     "DRUG",   "DRUG",  20,  0,    2
  ) %>%
    nif()

  result <- make_time_from_TIME(test_data)

  # Check that function handles missing values
  expect_true(all(c("TAFD", "TAD") %in% names(result)))
  expect_s3_class(result, "nif")

  # The function should handle missing TIME values
  # The exact behavior depends on how the function handles missing values
  expect_equal(length(result$TIME), 3)
})


test_that("make_time_from_TIME handles predose values with downup fill correctly", {
  # Test that downup fill allows negative TAD for pre-dose observations
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~ANALYTE, ~PARENT, ~DV, ~AMT, ~CMT,
    1,   -1,    0,     "A",      "A",     1,   0,    2,
    1,   -0.5,  0,     "A",      "A",     2,   0,    2,
    1,   0,     1,     "A",      "A",     NA, 100,  1,
    1,   0.5,   0,     "A",      "A",     3,   0,    2,
    1,   1,     1,     "A",      "A",     NA, 100,  1,
    1,   1.5,   0,     "A",      "A",     4,   0,    2
  ) %>%
    nif()

  result <- make_time_from_TIME(test_data) %>%
    as.data.frame()

  # Check TIME values (preserved as-is)
  expect_equal(result$TIME, c(-1, -0.5, 0, 0.5, 1, 1.5))

  # Check TAFD values
  expect_equal(result$TAFD, c(-1, -0.5, 0, 0.5, 1, 1.5))

  # Check TAD values (with downup fill, pre-dose should have negative TAD)
  # First dose at TIME=0, second at TIME=1
  expect_equal(result$TAD, c(-1, -0.5, 0, 0.5, 0, 0.5))
})


test_that("make_time_from_TIME works when PARENT column is missing but can be created", {
  # Test that ensure_parent is called and works
  # If PARENT can be created from ANALYTE or CMT, it should work
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~ANALYTE, ~DV,  ~AMT, ~CMT,
    1,   0,     1,     "DRUG",   NA,  100,  1,
    1,   1,     0,     "DRUG",   10,  0,    2
  ) %>%
    nif()

  # This should work if ensure_parent can create PARENT from ANALYTE
  # If it can't, it will error, which is expected behavior
  result <- make_time_from_TIME(test_data)

  # If successful, check that PARENT exists and calculations work
  expect_true("PARENT" %in% names(result))
  expect_true(all(c("TAFD", "TAD") %in% names(result)))
})


test_that("make_time_from_TIME handles multiple doses with observations in between", {
  # Test scenario with multiple doses and observations between them
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~ANALYTE, ~PARENT, ~DV,  ~AMT, ~CMT,
    1,   0,     1,     "DRUG",   "DRUG",  NA,  100,  1,   # First dose
    1,   0.5,   0,     "DRUG",   "DRUG",  5,   0,    2,   # Obs after first dose
    1,   1,     0,     "DRUG",   "DRUG",  10,  0,    2,   # Obs after first dose
    1,   2,     1,     "DRUG",   "DRUG",  NA,  100,  1,   # Second dose
    1,   2.5,   0,     "DRUG",   "DRUG",  15,  0,    2,   # Obs after second dose
    1,   3,     0,     "DRUG",   "DRUG",  20,  0,    2   # Obs after second dose
  ) %>%
    nif()

  result <- make_time_from_TIME(test_data)

  # Check TIME values
  expect_equal(result$TIME, c(0, 0.5, 1, 2, 2.5, 3))

  # Check TAFD values (all relative to first dose at TIME=0)
  expect_equal(result$TAFD, c(0, 0.5, 1, 2, 2.5, 3))

  # Check TAD values (relative to most recent dose)
  # First dose at TIME=0, second at TIME=2
  expect_equal(result$TAD, c(0, 0.5, 1, 0, 0.5, 1))
})


test_that("make_time_from_TIME handles unsorted input data", {
  # Test that function properly sorts data before calculations
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~ANALYTE, ~PARENT, ~DV,  ~AMT, ~CMT,
    1,   3,     0,     "DRUG",   "DRUG",  15,  0,    2,
    1,   1,     0,     "DRUG",   "DRUG",  10,  0,    2,
    1,   0,     1,     "DRUG",   "DRUG",  NA,  100,  1,
    1,   2,     0,     "DRUG",   "DRUG",  20,  0,    2
  ) %>%
    nif()

  result <- make_time_from_TIME(test_data)

  # Check that data is properly sorted and calculations are correct
  expect_equal(result$TIME, c(0, 1, 2, 3))
  expect_equal(result$TAFD, c(0, 1, 2, 3))
  expect_equal(result$TAD, c(0, 1, 2, 3))
})






