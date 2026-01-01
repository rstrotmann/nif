test_that("derive_baseline works with valid input", {
  # Create a simple test dataset
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV,  ~ANALYTE, ~EVID, ~TAFD, ~AMT, ~CMT,
    1,   -1,    10,   "A",      0,     -1,   0,    2,
    1,   0,     12,   "A",      0,     0,    0,    2,
    1,   1,     15,   "A",      0,     1,    0,    2,
    2,   -1,    20,   "A",      0,     -1,   0,    2,
    2,   0,     22,   "A",      0,     0,    0,    2,
    2,   1,     25,   "A",      0,     1,    0,    2
  )

  test_nif <- nif(test_data)

  # Run derive_baseline
  result <- derive_baseline(test_nif)

  # Check if DVBL column is created
  expect_true("DVBL" %in% names(result))

  # Check baseline values (should be median of pre-dose values)
  expect_equal(result$DVBL[result$ID == 1 & result$ANALYTE == "A"][1], 11) # ID 1 baseline (median of 10 and 12)
  expect_equal(result$DVBL[result$ID == 2 & result$ANALYTE == "A"][1], 21) # ID 2 baseline (median of 20 and 22)
})


test_that("derive_baseline handles empty baseline sets correctly", {
  # Create test data where baseline filter matches no rows
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV,  ~ANALYTE, ~EVID, ~TAFD, ~AMT, ~CMT,
    1,   1,     10,   "A",      0,     1,    0,    2,
    1,   2,     12,   "A",      0,     2,    0,    2,
    1,   3,     15,   "A",      0,     3,    0,    2,
    2,   1,     20,   "A",      0,     1,    0,    2,
    2,   2,     22,   "A",      0,     2,    0,    2,
    2,   3,     25,   "A",      0,     3,    0,    2
  )

  test_nif <- nif(test_data)

  # Test with filter that matches no rows (all times are positive)
  result <- derive_baseline(test_nif, baseline_filter = "TAFD < 0")

  # Check that DVBL uses default_baseline (NA_real_) for all rows
  expect_true(all(is.na(result$DVBL)))
})


test_that("derive_baseline handles empty baseline sets with custom default_baseline", {
  # Create test data where baseline filter matches no rows
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV,  ~ANALYTE, ~EVID, ~AMT, ~CMT,
    1,   1,     10,   "A",      0,     0,    2,
    1,   2,     12,   "A",      0,     0,    2
  ) %>%
    mutate(TAFD = TIME)

  test_nif <- nif(test_data)

  # Test with filter that matches no rows and custom default
  result <- derive_baseline(
    test_nif,
    baseline_filter = "TAFD < 0",
    default_baseline = 0
  )

  # Check that DVBL uses custom default_baseline (0) for all rows
  expect_equal(result$DVBL, c(0, 0))
})


test_that("derive_baseline handles all NA values in filtered baseline", {
  # Create test data where baseline filter matches rows but all DV values are NA
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV,  ~ANALYTE, ~EVID, ~AMT, ~CMT,
    1,   -1,    NA,   "A",      0,     0,    2,
    1,   0,     NA,   "A",      0,     0,    2,
    1,   1,     15,   "A",      0,     0,    2,
    2,   -1,    NA,   "A",      0,     0,    2,
    2,   0,     NA,   "A",      0,     0,    2,
    2,   1,     25,   "A",      0,     0,    2
  ) %>%
    mutate(TAFD = TIME)

  test_nif <- nif(test_data)

  # Test with filter that matches rows but all values are NA
  result <- derive_baseline(test_nif, baseline_filter = "TAFD <= 0")

  # Check that DVBL uses default_baseline (NA_real_) for all rows
  expect_true(all(is.na(result$DVBL)))
})


test_that("derive_baseline handles empty vector from summary function", {
  # Create test data where baseline filter matches rows but after na.omit, vector is empty
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV,  ~ANALYTE, ~EVID, ~AMT, ~CMT,
    1,   -1,    NA,   "A",      0,     0,    2,
    1,   0,     NA,   "A",      0,     0,    2,
    1,   1,     15,   "A",      0,     0,    2
  ) %>%
    mutate(TAFD = TIME)

  test_nif <- nif(test_data)

  # Test with filter that matches rows but all non-NA values are filtered out
  result <- derive_baseline(
    test_nif,
    baseline_filter = "TAFD <= 0",
    default_baseline = 999
  )

  # Check that DVBL uses default_baseline (999) when vector is empty
  expect_equal(result$DVBL[result$TAFD <= 0], c(999, 999))
  expect_equal(result$DVBL[result$TAFD == 1], 999)
})


test_that("derive_baseline works with different summary functions", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV,  ~ANALYTE, ~EVID, ~AMT, ~CMT,
    1,   -1,    10,   "A",      0,     0,    2,
    1,   0,     12,   "A",      0,     0,    2,
    1,   1,     15,   "A",      0,     0,    2
  ) %>%
    mutate(TAFD = TIME)

  test_nif <- nif(test_data)

  # Test with mean
  result_mean <- derive_baseline(test_nif, summary_function = mean)
  expect_equal(result_mean$DVBL[result_mean$TAFD <= 0][1], 11) # Mean of 10 and 12

  # Test with min
  result_min <- derive_baseline(test_nif, summary_function = min)
  expect_equal(result_min$DVBL[result_min$TAFD <= 0][1], 10) # Min of 10 and 12

  # Test with max
  result_max <- derive_baseline(test_nif, summary_function = max)
  expect_equal(result_max$DVBL[result_max$TAFD <= 0][1], 12) # Max of 10 and 12
})


test_that("derive_baseline handles multiple analytes", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV,  ~ANALYTE, ~EVID, ~AMT, ~CMT,
    1,   -1,    10,   "A",      0,     0,    2,
    1,   0,     12,   "A",      0,     0,    2,
    1,   1,     15,   "A",      0,     0,    2,
    1,   -1,    20,   "B",      0,     0,    3,
    1,   0,     22,   "B",      0,     0,    3,
    1,   1,     25,   "B",      0,     0,    3
  ) %>%
    mutate(TAFD = TIME)

  test_nif <- nif(test_data)

  result <- derive_baseline(test_nif)

  # Check baseline values for each analyte
  expect_equal(
    result$DVBL[result$ID == 1 & result$ANALYTE == "A" & result$TAFD <= 0][1],
    11
  )
  expect_equal(
    result$DVBL[result$ID == 1 & result$ANALYTE == "B" & result$TAFD <= 0][1],
    21
  )
})


test_that("derive_baseline handles specific analyte selection", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV,  ~ANALYTE, ~EVID, ~AMT, ~CMT,
    1,   -1,    10,   "A",      0,     0,    2,
    1,   0,     12,   "A",      0,     0,    2,
    1,   1,     15,   "A",      0,     0,    2,
    1,   -1,    20,   "B",      0,     0,    3,
    1,   0,     22,   "B",      0,     0,    3,
    1,   1,     25,   "B",      0,     0,    3
  ) %>%
    mutate(TAFD = TIME)

  test_nif <- nif(test_data)

  # Test with specific analyte
  result <- derive_baseline(test_nif, analyte = "A")

  # Check that only analyte A has baseline calculated
  expect_false(any(is.na(result$DVBL[result$ANALYTE == "A" & result$TAFD <= 0])))
  # Analyte B should not have baseline (or should be NA)
  expect_true(all(is.na(result$DVBL[result$ANALYTE == "B"])))
})


test_that("derive_baseline handles custom baseline filter", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV,  ~ANALYTE, ~EVID, ~AMT, ~CMT,
    1,   -1,    10,   "A",      0,     0,    2,
    1,   0,     12,   "A",      0,     0,    2,
    1,   1,     15,   "A",      0,     0,    2,
    1,   2,     18,   "A",      0,     0,    2
  ) %>%
    mutate(TAFD = TIME)

  test_nif <- nif(test_data)

  # Test with custom baseline filter
  result <- derive_baseline(test_nif, baseline_filter = "TAFD == 1")
  expect_equal(result$DVBL[result$TAFD == 1][1], 15) # Median of value at TAFD == 1
})


test_that("derive_baseline handles missing required columns", {
  # Test with missing DV column
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~ANALYTE, ~EVID, ~AMT, ~CMT,
    1,   -1,    "A",      0,     0,    2,
    1,   0,     "A",      0,     0,    2
  )
  test_nif <- nif(test_data)
  expect_error(derive_baseline(test_nif), "Missing required columns: DV")

  # Test with missing TIME column
  test_data <- tibble::tribble(
    ~ID, ~DV,  ~ANALYTE, ~EVID, ~AMT, ~CMT,
    1,   10,   "A",      0,     0,    2,
    1,   12,   "A",      0,     0,    2
  )
  test_nif <- nif(test_data)
  expect_error(derive_baseline(test_nif), "Missing required columns: TIME")
})


test_that("derive_baseline handles non-numeric columns", {
  # Test with non-numeric DV
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV,  ~ANALYTE, ~EVID, ~AMT, ~CMT,
    1,   -1,    "A",  "A",      0,     0,    2,
    1,   0,     "B",  "A",      0,     0,    2
  ) %>%
    mutate(TAFD = TIME)

  test_nif <- nif(test_data)
  expect_error(derive_baseline(test_nif), "DV column must contain numeric values")

  # Test with non-numeric TIME
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV,  ~ANALYTE, ~EVID, ~AMT, ~CMT,
    1,   "A",   10,   "A",      0,     0,    2,
    1,   "B",   12,   "A",      0,     0,    2
  ) %>%
    mutate(TAFD = TIME)

  test_nif <- nif(test_data)
  expect_error(derive_baseline(test_nif), "TIME column must contain numeric values")
})


test_that("derive_baseline handles EVID filtering correctly", {
  # Create test data with both EVID 0 and 1
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV,  ~ANALYTE, ~EVID, ~AMT, ~CMT,
    1,   -1,    8,    "A",      0,     0,    2,
    1,   0,     10,   "A",      1,     100,  1,   # Dosing event
    1,   1,     12,   "A",      0,     0,    2,
    2,   -1,    18,   "A",      0,     0,    2,
    2,   0,     20,   "A",      1,     100,  1,   # Dosing event
    2,   1,     22,   "A",      0,     0,    2
  ) %>%
    mutate(TAFD = TIME)

  test_nif <- nif(test_data)

  # Baseline should only use EVID == 0 rows
  result <- derive_baseline(test_nif, baseline_filter = "TAFD <= 0")

  # Check baseline values (should only use EVID == 0, so only TAFD == -1)
  expect_equal(result$DVBL[result$ID == 1 & result$TAFD <= 0 & result$EVID == 0][1], 8)
  expect_equal(result$DVBL[result$ID == 2 & result$TAFD <= 0 & result$EVID == 0][1], 18)
})

