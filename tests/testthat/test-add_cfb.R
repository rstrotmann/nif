test_that("derive_cfb works with valid input", {
  # Create a simple test dataset
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~ANALYTE, ~AMT, ~CMT, ~EVID,
    1,   -1,    10,  "A",      0,    1,    0,
    1,   0,     12,  "A",      0,    1,    0,
    1,   1,     15,  "A",      0,    1,    0,
    2,   -1,    20,  "A",      0,    1,    0,
    2,   0,     22,  "A",      0,    1,    0,
    2,   1,     25,  "A",      0,    1,    0
  ) %>%
    mutate(TAFD = TIME)

  test_nif <- nif(test_data)

  # Run derive_cfb
  result <- derive_cfb(test_nif)

  # Check if new columns are created
  expect_true("DVBL" %in% names(result))
  expect_true("DVCFB" %in% names(result))

  # Check baseline values (should be mean of pre-dose and time 0 values)
  expect_equal(result$DVBL[1], 11) # ID 1 baseline (mean of 10 and 12)
  expect_equal(result$DVBL[4], 21) # ID 2 baseline (mean of 20 and 22)

  # Check change from baseline values
  expect_equal(result$DVCFB, c(-1, 1, 4, -1, 1, 4))
})


test_that("derive_cfb correctly handles baseline calculation with time ≤ 0", {
  # Create test data with multiple pre-dose values
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~ANALYTE, ~AMT, ~CMT, ~EVID,
    1,   -2,    8,   "A",      0,    1,    0,
    1,   -1,    10,  "A",      0,    1,    0,
    1,   0,     12,  "A",      0,    1,    0,
    1,   1,     15,  "A",      0,    1,    0,
    2,   -2,    18,  "A",      0,    1,    0,
    2,   -1,    20,  "A",      0,    1,    0,
    2,   0,     22,  "A",      0,    1,    0,
    2,   1,     25,  "A",      0,    1,    0
  ) %>%
    mutate(TAFD = TIME)

  test_nif <- nif(test_data)
  result <- derive_cfb(test_nif, baseline_filter = "TIME <= 0")

  # Check baseline values (should be mean of all values with time ≤ 0)
  expect_equal(result$DVBL[1], 10) # ID 1 baseline (mean of 8, 10, and 12)
  expect_equal(result$DVBL[5], 20) # ID 2 baseline (mean of 18, 20, and 22)

  # Check change from baseline values
  expect_equal(result$DVCFB[1], -2) # Time -2
  expect_equal(result$DVCFB[2], 0) # Time -1
  expect_equal(result$DVCFB[3], 2) # Time 0
  expect_equal(result$DVCFB[4], 5) # Time 1
  expect_equal(result$DVCFB[5], -2) # Time -2
  expect_equal(result$DVCFB[6], 0) # Time -1
  expect_equal(result$DVCFB[7], 2) # Time 0
  expect_equal(result$DVCFB[8], 5) # Time 1
})


# test_that("derive_cfb handles NA values in grouping columns", {
#   # Create test data with NA values
#   test_data <- tibble::tribble(
#     ~ID, ~TIME, ~DV, ~ANALYTE,
#       1,    -1,  10,      "A",
#       1,     0,  12,      "A",
#       1,     1,  15,      "A",
#       2,    -1,  20,      "A",
#       2,     0,  22,      "A",
#       2,     1,  25,      "A",
#       NA,    0,  30,      "B",
#       3,     0,  35,       NA
#   ) %>%
#     mutate(EVID = 0) %>%
#     mutate(TAFD = TIME)
#
#   test_nif <- nif(test_data)
#
#   # Run derive_cfb and expect message
#   expect_message(
#     expect_message(
#       result <- derive_cfb(test_nif, silent = FALSE),
#       "Found NA values in ID column"),
#     "Found NA values in ANALYTE column")
#
#   # Check that rows with NA in grouping columns are excluded
#   expect_equal(nrow(result), 6)  # Only rows with valid ID and ANALYTE
# })


test_that("derive_cfb works with different summary functions", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~ANALYTE, ~AMT, ~CMT, ~EVID,
    1,   -1,    10,  "A",      0,    1,    0,
    1,   0,     12,  "A",      0,    1,    0,
    1,   1,     15,  "A",      0,    1,    0
  ) %>%
    mutate(TAFD = TIME)

  test_nif <- nif(test_data)

  # Test with mean
  result_mean <- derive_cfb(test_nif, summary_function = mean)
  expect_equal(result_mean$DVBL[1], 11) # Mean of pre-dose values

  # Test with min
  result_min <- derive_cfb(test_nif, summary_function = min)
  expect_equal(result_min$DVBL[1], 10) # Min of pre-dose values

  # Test with max
  result_max <- derive_cfb(test_nif, summary_function = max)
  expect_equal(result_max$DVBL[1], 12) # Max of pre-dose values
})


test_that("derive_cfb works with custom baseline filter", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~ANALYTE, ~AMT, ~CMT, ~EVID,
    1,   -1,    10,  "A",      0,    1,    0,
    1,   0,     12,  "A",      0,    1,    0,
    1,   1,     15,  "A",      0,    1,    0,
    1,   2,     18,  "A",      0,    1,    0
  ) %>%
    mutate(TAFD = TIME)

  test_nif <- nif(test_data)

  # Test with custom baseline filter
  result <- derive_cfb(test_nif, baseline_filter = "TIME == 1")
  expect_equal(result$DVBL[1], 15) # Median of values at TIME <= 1
})


test_that("derive_cfb handles missing required columns", {
  # Test with missing DV column
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~ANALYTE, ~AMT, ~CMT, ~EVID, ~DV,
    1,   -1,    "A",      0,    1,    0,     NA,
    1,   0,     "A",      0,    1,    0,     NA
  )
  test_nif <- nif(test_data)
  test_nif <- select(test_nif, -DV)
  expect_error(derive_cfb(test_nif), "Missing required columns: DV")


  test_nif <- nif(test_data)
  test_nif <- select(test_nif, -TIME)
  expect_error(derive_cfb(test_nif), "Missing required columns: TIME")
})


test_that("derive_cfb handles non-numeric columns", {
  # Test with non-numeric DV
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~ANALYTE, ~AMT, ~CMT, ~EVID,
    1,   -1,    "A", "A",      0,    1,    0,
    1,   0,     "B", "A",      0,    1,    0
  ) %>%
    mutate(TAFD = TIME)

  test_nif <- nif(test_data)
  expect_error(derive_cfb(test_nif), "DV column must contain numeric values")

  # Test with non-numeric TIME
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~ANALYTE, ~AMT, ~CMT, ~EVID,
    1,   "A",   10,  "A",      0,    1,    0,
    1,   "B",   12,  "A",      0,    1,    0
  ) %>%
    mutate(TAFD = TIME)

  test_nif <- nif(test_data)
  expect_error(derive_cfb(test_nif), "TIME column must contain numeric values")
})


test_that("derive_cfb correctly handles complex baseline filters", {
  # Create test data with multiple conditions
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~ANALYTE, ~AMT, ~CMT, ~EVID,
    1,   -2,    8,   "A",      0,    1,    0,
    1,   -1,    10,  "A",      0,    1,    0,
    1,   0,     12,  "A",      0,    1,    1,
    1,   1,     15,  "A",      0,    1,    0,
    2,   -2,    18,  "A",      0,    1,    0,
    2,   -1,    20,  "A",      0,    1,    0,
    2,   0,     22,  "A",      0,    1,    1,
    2,   1,     25,  "A",      0,    1,    0
  )

  test_nif <- nif(test_data)

  # Test filter combining multiple conditions
  result <- derive_cfb(test_nif, baseline_filter = "TIME <= 0 & EVID == 0")

  # Check baseline values (should only use pre-dose values, excluding EVID=1)
  expect_equal(result$DVBL[1], 9) # ID 1 baseline (mean of 8 and 10)
  expect_equal(result$DVBL[5], 19) # ID 2 baseline (mean of 18 and 20)

  # Check change from baseline values
  expect_equal(result$DVCFB[1], -1) # Time -2
  expect_equal(result$DVCFB[2], 1) # Time -1
  expect_equal(result$DVCFB[3], 3) # Time 0 (EVID=1)
  expect_equal(result$DVCFB[4], 6) # Time 1
  expect_equal(result$DVCFB[5], -1) # Time -2
  expect_equal(result$DVCFB[6], 1) # Time -1
  expect_equal(result$DVCFB[7], 3) # Time 0 (EVID=1)
  expect_equal(result$DVCFB[8], 6) # Time 1
})


test_that("derive_cfb correctly handles empty baseline sets", {
  # Create test data where baseline filter matches no rows
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~ANALYTE, ~AMT, ~CMT, ~EVID,
    1,   1,     10,  "A",      0,    1,    0,
    1,   2,     12,  "A",      0,    1,    0,
    1,   3,     15,  "A",      0,    1,    0,
    2,   1,     20,  "A",      0,    1,    0,
    2,   2,     22,  "A",      0,    1,    0,
    2,   3,     25,  "A",      0,    1,    0
  ) %>%
    mutate(TAFD = TIME)

  test_nif <- nif(test_data)

  # Test with filter that matches no rows
  result <- derive_cfb(test_nif, baseline_filter = "TIME < 0")

  # Check that DVBL is NA for all rows
  expect_true(all(is.na(result$DVBL)))
  # Check that DVCFB is NA for all rows
  expect_true(all(is.na(result$DVCFB)))
})


test_that("derive_cfb correctly handles baseline filter with missing values", {
  # Create test data with missing values in filter columns
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~ANALYTE, ~AMT, ~CMT, ~EVID,
    1,   -1,    10,  "A",      0,    1,    0,
    1,   NA,    12,  "A",      0,    1,    0,
    1,   1,     15,  "A",      0,    1,    0,
    2,   -1,    20,  "A",      0,    1,    0,
    2,   NA,    22,  "A",      0,    1,    0,
    2,   1,     25,  "A",      0,    1,    0
  ) %>%
    mutate(TAFD = TIME)

  test_nif <- nif(test_data)

  # Test with filter that includes NA values
  result <- derive_cfb(test_nif, baseline_filter = "TIME <= 0 | is.na(TIME)")

  # Check baseline values (should include NA values)
  expect_equal(result$DVBL[1], 11) # ID 1 baseline (mean of 10 and 12)
  expect_equal(result$DVBL[4], 21) # ID 2 baseline (mean of 20 and 22)

  # Check change from baseline values
  expect_equal(result$DVCFB[1], -1) # Time -1
  expect_equal(result$DVCFB[2], 1) # Time NA
  expect_equal(result$DVCFB[3], 4) # Time 1
  expect_equal(result$DVCFB[4], -1) # Time -1
  expect_equal(result$DVCFB[5], 1) # Time NA
  expect_equal(result$DVCFB[6], 4) # Time 1
})


test_that("derive_cfb correctly handles baseline filter with character columns", {
  # Create test data with character columns and multiple conditions
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~ANALYTE, ~FOOD,    ~AMT, ~CMT, ~EVID,
    1,   -1,    10,  "A",      "FED",    0,    1,    0,
    1,   0,     12,  "A",      "FED",    0,    1,    0,
    1,   1,     15,  "A",      "FASTED", 0,    1,    0,
    1,   2,     18,  "A",      "FASTED", 0,    1,    0,
    2,   -1,    20,  "A",      "FASTED", 0,    1,    0,
    2,   0,     22,  "A",      "FASTED", 0,    1,    0,
    2,   1,     25,  "A",      "FED",    0,    1,    0,
    2,   2,     28,  "A",      "FED",    0,    1,    0
  ) %>%
    mutate(TAFD = TIME)

  test_nif <- nif(test_data)

  # Test 1: Filter using character column with single condition
  result1 <- derive_cfb(test_nif, baseline_filter = "FOOD == 'FED'")

  # Check baseline values (should use all FED values)
  expect_equal(result1$DVBL[1], 11) # ID 1 baseline (mean of 10 and 12)
  expect_equal(result1$DVBL[5], 26.5) # ID 2 baseline (mean of 25 and 28)

  # Test 2: Filter using character column with multiple conditions
  result2 <- derive_cfb(test_nif, baseline_filter = "TIME <= 0 & FOOD == 'FED'")

  # Check baseline values (should only use pre-dose FED values)
  expect_equal(result2$DVBL[1], 11) # ID 1 baseline (only pre-dose FED value)
  expect_true(is.na(result2$DVBL[5])) # ID 2 baseline (only post-dose FED value)

  # Test 3: Filter using character column with case sensitivity
  result3 <- derive_cfb(test_nif, baseline_filter = "FOOD == 'fed'")

  # Should return NA for baseline since case-sensitive comparison fails
  expect_true(all(is.na(result3$DVBL)))

  # Test 4: Filter using character column with NA values
  test_data_na <- test_data
  test_data_na$FOOD[1] <- NA
  test_nif_na <- nif(test_data_na)

  result4 <- derive_cfb(test_nif_na, baseline_filter = "FOOD == 'FED'")

  # Should exclude NA values from baseline calculation
  expect_equal(result4$DVBL[2], 12) # ID 1 baseline (only non-NA FED value)
  expect_equal(result4$DVBL[5], 26.5) # ID 2 baseline (mean of 25 and 28)
})
