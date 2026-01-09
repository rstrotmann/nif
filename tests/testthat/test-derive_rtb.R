# Comprehensive tests for derive_rtb() function

test_that("derive_rtb works with valid input", {
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

  # Run derive_rtb
  result <- derive_rtb(test_nif)

  # Check if new columns are created
  expect_true("DVBL" %in% names(result))
  expect_true("DVRTB" %in% names(result))

  # Check baseline values (should be median of pre-dose values)
  expect_equal(result$DVBL[result$ID == 1 & result$ANALYTE == "A"][1], 11) # ID 1 baseline (median of 10 and 12)
  expect_equal(result$DVBL[result$ID == 2 & result$ANALYTE == "A"][1], 21) # ID 2 baseline (median of 20 and 22)

  # Check ratio to baseline values (DVRTB = DV / DVBL)
  expect_equal(result$DVRTB[result$ID == 1 & result$TIME == -1], 10 / 11) # 10 / 11
  expect_equal(result$DVRTB[result$ID == 1 & result$TIME == 0], 12 / 11)    # 12 / 11
  expect_equal(result$DVRTB[result$ID == 1 & result$TIME == 1], 15 / 11)   # 15 / 11
  expect_equal(result$DVRTB[result$ID == 2 & result$TIME == -1], 20 / 21)  # 20 / 21
  expect_equal(result$DVRTB[result$ID == 2 & result$TIME == 0], 22 / 21)   # 22 / 21
  expect_equal(result$DVRTB[result$ID == 2 & result$TIME == 1], 25 / 21)   # 25 / 21
})


test_that("derive_rtb correctly handles baseline calculation with TAFD <= 0", {
  # Create test data with multiple pre-dose values
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV,  ~ANALYTE, ~EVID, ~AMT, ~CMT,
    1,   -2,    8,    "A",      0,     0,    2,
    1,   -1,    10,   "A",      0,     0,    2,
    1,   0,     12,   "A",      0,     0,    2,
    1,   1,     15,   "A",      0,     0,    2,
    2,   -2,    18,   "A",      0,     0,    2,
    2,   -1,    20,   "A",      0,     0,    2,
    2,   0,     22,   "A",      0,     0,    2,
    2,   1,     25,   "A",      0,     0,    2
  ) %>%
    mutate(TAFD = TIME)

  test_nif <- nif(test_data)
  result <- derive_rtb(test_nif)

  # Check baseline values (should be median of all values with TAFD <= 0)
  expect_equal(result$DVBL[result$ID == 1 & result$TAFD <= 0][1], 10) # ID 1 baseline (median of 8, 10, and 12)
  expect_equal(result$DVBL[result$ID == 2 & result$TAFD <= 0][1], 20) # ID 2 baseline (median of 18, 20, and 22)

  # Check ratio to baseline values
  expect_equal(result$DVRTB[result$ID == 1 & result$TAFD == -2], 8 / 10)  # 8 / 10
  expect_equal(result$DVRTB[result$ID == 1 & result$TAFD == -1], 10 / 10) # 10 / 10 = 1
  expect_equal(result$DVRTB[result$ID == 1 & result$TAFD == 0], 12 / 10)   # 12 / 10
  expect_equal(result$DVRTB[result$ID == 1 & result$TAFD == 1], 15 / 10)  # 15 / 10
  expect_equal(result$DVRTB[result$ID == 2 & result$TAFD == -2], 18 / 20) # 18 / 20
  expect_equal(result$DVRTB[result$ID == 2 & result$TAFD == -1], 20 / 20) # 20 / 20 = 1
  expect_equal(result$DVRTB[result$ID == 2 & result$TAFD == 0], 22 / 20)   # 22 / 20
  expect_equal(result$DVRTB[result$ID == 2 & result$TAFD == 1], 25 / 20)   # 25 / 20
})


test_that("derive_rtb handles empty baseline sets correctly", {
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
  result <- derive_rtb(test_nif, baseline_filter = "TAFD < 0")

  # Check that DVBL uses default_baseline (NA_real_) for all rows
  expect_true(all(is.na(result$DVBL)))
  # Check that DVRTB is also NA when DVBL is NA
  expect_true(all(is.na(result$DVRTB)))
})


test_that("derive_rtb handles empty baseline sets with custom default_baseline", {
  # Create test data where baseline filter matches no rows
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV,  ~ANALYTE, ~EVID, ~AMT, ~CMT,
    1,   1,     10,   "A",      0,     0,    2,
    1,   2,     12,   "A",      0,     0,    2
  ) %>%
    mutate(TAFD = TIME)

  test_nif <- nif(test_data)

  # Test with filter that matches no rows and custom default
  result <- derive_rtb(
    test_nif,
    baseline_filter = "TAFD < 0",
    default_baseline = 0
  )

  # Check that DVBL uses custom default_baseline (0) for all rows
  expect_equal(result$DVBL, c(0, 0))
  # Check that DVRTB is NA when DVBL is 0 (division by zero)
  expect_true(all(is.na(result$DVRTB)))
})


test_that("derive_rtb handles all NA values in filtered baseline", {
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
  result <- derive_rtb(test_nif)

  # Check that DVBL uses default_baseline (NA_real_) for all rows
  expect_true(all(is.na(result$DVBL)))
  # Check that DVRTB is also NA when DVBL is NA
  expect_true(all(is.na(result$DVRTB)))
})


test_that("derive_rtb handles empty vector from summary function", {
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
  result <- derive_rtb(
    test_nif,
    default_baseline = 999
  )

  # Check that DVBL uses default_baseline (999) when vector is empty
  expect_equal(result$DVBL[result$TAFD <= 0], c(999, 999))
  expect_equal(result$DVBL[result$TAFD == 1], 999)
  # Check that DVRTB is calculated correctly
  expect_equal(result$DVRTB[result$TAFD == 1], 15 / 999) # DV / default_baseline
})


test_that("derive_rtb works with different summary functions", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV,  ~ANALYTE, ~EVID, ~AMT, ~CMT,
    1,   -1,    10,   "A",      0,     0,    2,
    1,   0,     12,   "A",      0,     0,    2,
    1,   1,     15,   "A",      0,     0,    2
  ) %>%
    mutate(TAFD = TIME)

  test_nif <- nif(test_data)

  # Test with mean
  result_mean <- derive_rtb(test_nif, summary_function = mean)
  expect_equal(result_mean$DVBL[result_mean$TAFD <= 0][1], 11) # Mean of 10 and 12
  expect_equal(result_mean$DVRTB[result_mean$TAFD == 1], 15 / 11) # 15 / 11

  # Test with min
  result_min <- derive_rtb(test_nif, summary_function = min)
  expect_equal(result_min$DVBL[result_min$TAFD <= 0][1], 10) # Min of 10 and 12
  expect_equal(result_min$DVRTB[result_min$TAFD == 1], 15 / 10) # 15 / 10 = 1.5

  # Test with max
  result_max <- derive_rtb(test_nif, summary_function = max)
  expect_equal(result_max$DVBL[result_max$TAFD <= 0][1], 12) # Max of 10 and 12
  expect_equal(result_max$DVRTB[result_max$TAFD == 1], 15 / 12) # 15 / 12 = 1.25
})


test_that("derive_rtb handles multiple analytes", {
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

  result <- derive_rtb(test_nif)

  # Check baseline values for each analyte
  expect_equal(
    result$DVBL[result$ID == 1 & result$ANALYTE == "A" & result$TAFD <= 0][1],
    11
  )
  expect_equal(
    result$DVBL[result$ID == 1 & result$ANALYTE == "B" & result$TAFD <= 0][1],
    21
  )

  # Check ratio to baseline values for each analyte
  expect_equal(
    result$DVRTB[result$ID == 1 & result$ANALYTE == "A" & result$TAFD == 1],
    15 / 11  # 15 / 11
  )
  expect_equal(
    result$DVRTB[result$ID == 1 & result$ANALYTE == "B" & result$TAFD == 1],
    25 / 21  # 25 / 21
  )
})


test_that("derive_rtb handles specific analyte selection", {
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
  result <- derive_rtb(test_nif, analyte = "A")

  # Check that only analyte A has baseline calculated
  expect_false(any(is.na(result$DVBL[result$ANALYTE == "A" & result$TAFD <= 0])))
  # Analyte B should not have baseline (or should be NA)
  expect_true(all(is.na(result$DVBL[result$ANALYTE == "B"])))
  # Analyte B should not have DVRTB calculated
  expect_true(all(is.na(result$DVRTB[result$ANALYTE == "B"])))
})


test_that("derive_rtb handles custom baseline filter", {
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
  result <- derive_rtb(test_nif, baseline_filter = "TAFD == 1")
  expect_equal(result$DVBL[result$TAFD == 1][1], 15) # Median of value at TAFD == 1
  expect_equal(result$DVRTB[result$TAFD == 1][1], 15 / 15) # 1
})


test_that("derive_rtb handles EVID filtering correctly", {
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
  result <- derive_rtb(test_nif)

  # Check baseline values (should only use EVID == 0, so only TAFD == -1)
  expect_equal(result$DVBL[result$ID == 1 & result$TAFD <= 0 & result$EVID == 0][1], 8)
  expect_equal(result$DVBL[result$ID == 2 & result$TAFD <= 0 & result$EVID == 0][1], 18)
  # Check ratio to baseline for observations
  expect_equal(result$DVRTB[result$ID == 1 & result$TAFD == 1 & result$EVID == 0], 12 / 8) # 1.5
  expect_equal(result$DVRTB[result$ID == 2 & result$TAFD == 1 & result$EVID == 0], 22 / 18) # 22/18
})


test_that("derive_rtb correctly handles complex baseline filters", {
  # Create test data with multiple conditions
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV,  ~ANALYTE, ~EVID, ~AMT, ~CMT,
    1,   -2,    8,    "A",      0,     0,    2,
    1,   -1,    10,   "A",      0,     0,    2,
    1,   0,     12,   "A",      1,     100,  1,
    1,   1,     15,   "A",      0,     0,    2,
    2,   -2,    18,   "A",      0,     0,    2,
    2,   -1,    20,   "A",      0,     0,    2,
    2,   0,     22,   "A",      1,     100,  1,
    2,   1,     25,   "A",      0,     0,    2
  ) %>%
    mutate(TAFD = TIME)

  test_nif <- nif(test_data)

  # Test filter combining multiple conditions
  result <- derive_rtb(test_nif, baseline_filter = "TAFD <= 0 & EVID == 0")

  # Check baseline values (should only use pre-dose values, excluding EVID=1)
  expect_equal(result$DVBL[result$ID == 1 & result$TAFD <= 0 & result$EVID == 0][1], 9) # ID 1 baseline (median of 8 and 10)
  expect_equal(result$DVBL[result$ID == 2 & result$TAFD <= 0 & result$EVID == 0][1], 19) # ID 2 baseline (median of 18 and 20)

  # Check ratio to baseline values
  expect_equal(result$DVRTB[result$ID == 1 & result$TAFD == -2], 8 / 9)   # 8 / 9
  expect_equal(result$DVRTB[result$ID == 1 & result$TAFD == -1], 10 / 9)  # 10 / 9
  expect_equal(result$DVRTB[result$ID == 1 & result$TAFD == 1], 15 / 9)   # 15 / 9
  expect_equal(result$DVRTB[result$ID == 2 & result$TAFD == -2], 18 / 19) # 18 / 19
  expect_equal(result$DVRTB[result$ID == 2 & result$TAFD == -1], 20 / 19) # 20 / 19
  expect_equal(result$DVRTB[result$ID == 2 & result$TAFD == 1], 25 / 19)  # 25 / 19
})


test_that("derive_rtb correctly handles baseline filter with missing values", {
  # Create test data with missing values in filter columns
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV,  ~ANALYTE, ~EVID, ~AMT, ~CMT,
    1,   -1,    10,   "A",      0,     0,    2,
    1,   NA,    12,   "A",      0,     0,    2,
    1,   1,     15,   "A",      0,     0,    2,
    2,   -1,    20,   "A",      0,     0,    2,
    2,   NA,    22,   "A",      0,     0,    2,
    2,   1,     25,   "A",      0,     0,    2
  ) %>%
    mutate(TAFD = TIME)

  test_nif <- nif(test_data)

  # Test with filter that includes NA values
  result <- derive_rtb(test_nif, baseline_filter = "TAFD <= 0 | is.na(TAFD)")

  # Check baseline values (should include NA values)
  expect_equal(result$DVBL[result$ID == 1 & (result$TAFD <= 0 | is.na(result$TAFD))][1], 11) # ID 1 baseline (median of 10 and 12)
  expect_equal(result$DVBL[result$ID == 2 & (result$TAFD <= 0 | is.na(result$TAFD))][1], 21) # ID 2 baseline (median of 20 and 22)
})


test_that("derive_rtb correctly handles baseline filter with character columns", {
  # Create test data with character columns and multiple conditions
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV,  ~ANALYTE, ~FOOD,    ~EVID, ~AMT, ~CMT,
    1,   -1,    10,   "A",      "FED",    0,     0,    2,
    1,   0,     12,   "A",      "FED",    0,     0,    2,
    1,   1,     15,   "A",      "FASTED", 0,     0,    2,
    1,   2,     18,   "A",      "FASTED", 0,     0,    2,
    2,   -1,    20,   "A",      "FASTED", 0,     0,    2,
    2,   0,     22,   "A",      "FASTED", 0,     0,    2,
    2,   1,     25,   "A",      "FED",    0,     0,    2,
    2,   2,     28,   "A",      "FED",    0,     0,    2
  ) %>%
    mutate(TAFD = TIME)

  test_nif <- nif(test_data)

  # Test 1: Filter using character column with single condition
  result1 <- derive_rtb(test_nif, baseline_filter = "FOOD == 'FED'")

  # Check baseline values (should use all FED values)
  expect_equal(result1$DVBL[result1$ID == 1 & result1$FOOD == "FED"][1], 11) # ID 1 baseline (median of 10 and 12)
  expect_equal(result1$DVBL[result1$ID == 2 & result1$FOOD == "FED"][1], 26.5) # ID 2 baseline (median of 25 and 28)

  # Test 2: Filter using character column with multiple conditions
  result2 <- derive_rtb(test_nif, baseline_filter = "TAFD <= 0 & FOOD == 'FED'")

  # Check baseline values (should only use pre-dose FED values)
  expect_equal(result2$DVBL[result2$ID == 1 & result2$TAFD <= 0 & result2$FOOD == "FED"][1], 11) # ID 1 baseline (only pre-dose FED value)
  expect_true(is.na(result2$DVBL[result2$ID == 2 & result2$FOOD == "FED"][1])) # ID 2 baseline (only post-dose FED value)

  # Test 3: Filter using character column with case sensitivity
  result3 <- derive_rtb(test_nif, baseline_filter = "FOOD == 'fed'")

  # Should return NA for baseline since case-sensitive comparison fails
  expect_true(all(is.na(result3$DVBL)))

  # Test 4: Filter using character column with NA values
  test_data_na <- test_data
  test_data_na$FOOD[1] <- NA
  test_nif_na <- nif(test_data_na)

  result4 <- derive_rtb(test_nif_na, baseline_filter = "FOOD == 'FED'")

  # Should exclude NA values from baseline calculation
  expect_equal(result4$DVBL[result4$ID == 1 & result4$FOOD == "FED" & !is.na(result4$FOOD)][1], 12) # ID 1 baseline (only non-NA FED value)
  expect_equal(result4$DVBL[result4$ID == 2 & result4$FOOD == "FED"][1], 26.5) # ID 2 baseline (median of 25 and 28)
})


test_that("derive_rtb handles multiple subjects correctly", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV,  ~ANALYTE, ~EVID, ~AMT, ~CMT,
    1,   -1,    10,   "A",      0,     0,    2,
    1,   0,     12,   "A",      0,     0,    2,
    1,   1,     15,   "A",      0,     0,    2,
    2,   -1,    20,   "A",      0,     0,    2,
    2,   0,     22,   "A",      0,     0,    2,
    2,   1,     25,   "A",      0,     0,    2,
    3,   -1,    30,   "A",      0,     0,    2,
    3,   0,     32,   "A",      0,     0,    2,
    3,   1,     35,   "A",      0,     0,    2
  ) %>%
    mutate(TAFD = TIME)

  test_nif <- nif(test_data)
  result <- derive_rtb(test_nif)

  # Check that each subject has their own baseline
  expect_equal(result$DVBL[result$ID == 1 & result$TAFD <= 0][1], 11) # Subject 1 baseline
  expect_equal(result$DVBL[result$ID == 2 & result$TAFD <= 0][1], 21) # Subject 2 baseline
  expect_equal(result$DVBL[result$ID == 3 & result$TAFD <= 0][1], 31) # Subject 3 baseline

  # Check that ratio to baseline is calculated per subject
  expect_equal(result$DVRTB[result$ID == 1 & result$TAFD == 1], 15 / 11) # 15 / 11
  expect_equal(result$DVRTB[result$ID == 2 & result$TAFD == 1], 25 / 21) # 25 / 21
  expect_equal(result$DVRTB[result$ID == 3 & result$TAFD == 1], 35 / 31) # 35 / 31
})


test_that("derive_rtb preserves nif class", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV,  ~ANALYTE, ~EVID, ~AMT, ~CMT,
    1,   -1,    10,   "A",      0,     0,    2,
    1,   0,     12,   "A",      0,     0,    2,
    1,   1,     15,   "A",      0,     0,    2
  ) %>%
    mutate(TAFD = TIME)

  test_nif <- nif(test_data)
  result <- derive_rtb(test_nif)

  # Check that result is still a nif object
  expect_true(inherits(result, "nif"))
})


test_that("derive_rtb handles zero baseline values", {
  # Create test data where baseline is zero
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV,  ~ANALYTE, ~EVID, ~AMT, ~CMT,
    1,   -1,    0,    "A",      0,     0,    2,
    1,   0,     0,    "A",      0,     0,    2,
    1,   1,     5,    "A",      0,     0,    2
  ) %>%
    mutate(TAFD = TIME)

  test_nif <- nif(test_data)
  result <- derive_rtb(test_nif)

  # Check baseline is zero
  expect_equal(result$DVBL[result$TAFD <= 0][1], 0)
  # Check that DVRTB is NA when baseline is zero (division by zero)
  expect_true(is.na(result$DVRTB[result$TAFD == 1]))
})


test_that("derive_rtb handles negative DV values", {
  # Create test data with negative values
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV,  ~ANALYTE, ~EVID, ~AMT, ~CMT,
    1,   -1,    -10,  "A",      0,     0,    2,
    1,   0,     -12,  "A",      0,     0,    2,
    1,   1,     -15,  "A",      0,     0,    2
  ) %>%
    mutate(TAFD = TIME)

  test_nif <- nif(test_data)
  result <- derive_rtb(test_nif)

  # Check baseline with negative values
  expect_equal(result$DVBL[result$TAFD <= 0][1], -11) # Median of -10 and -12
  # Check ratio to baseline with negative values
  expect_equal(result$DVRTB[result$TAFD == 1], -15 / (-11)) # 15/11
})


test_that("derive_rtb handles silent parameter", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~DV,  ~ANALYTE, ~EVID, ~AMT, ~CMT,
    1,   -1,    10,   "A",      0,     0,    2,
    1,   0,     12,   "A",      0,     0,    2,
    1,   1,     15,   "A",      0,     0,    2
  ) %>%
    mutate(TAFD = TIME)

  test_nif <- nif(test_data)

  # Test with silent = TRUE (should not produce messages)
  expect_silent(result <- derive_rtb(test_nif, silent = TRUE))

  # Test with silent = FALSE (may produce messages, but should still work)
  result <- derive_rtb(test_nif, silent = FALSE)
  expect_true("DVBL" %in% names(result))
  expect_true("DVRTB" %in% names(result))
})
