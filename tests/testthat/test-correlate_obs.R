
test_that("correlate_obs input validation - non-nif objects", {
  # Test that non-nif objects are rejected
  expect_error(correlate_obs(data.frame()), "Input must be a nif object")

  # Test that regular data.frame is rejected
  test_df <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~EVID, ~ANALYTE, ~DTC, ~REF,
      1,     0,  10,     0,   "A",   "2023-01-01T00:00:00", 1
  )

  expect_error(correlate_obs(test_df), "Input must be a nif object")
})


test_that("correlate_obs validates REF field requirement", {
  # Create test nif object without REF
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~EVID, ~ANALYTE, ~DTC,
      1,     0,  10,     0,   "DRUG",   "2023-01-01T00:00:00"
  ) %>%
    nif() %>%
    lubrify_dates()

  # Remove REF if it exists
  if("REF" %in% names(test_nif)) {
    test_nif <- test_nif %>% select(-REF)
  }

  expect_error(
    correlate_obs(test_nif, indep_analyte = "DRUG", dep_analyte = "METAB"),
    "Input must contain the 'REF' field"
  )
})


test_that("correlate_obs validates analyte parameters", {
  # Create valid test nif object
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~EVID, ~ANALYTE, ~DTC, ~REF,
      1,     0,   0,     1,   "DRUG",   "2023-01-01T00:00:00", 1,
      1,     0,  10,     0,   "DRUG",   "2023-01-01T00:00:00", 2
  ) %>%
    nif() %>%
    lubrify_dates()

  # Test missing independent analyte
  expect_error(
    correlate_obs(test_nif, indep_analyte = "NONEXISTENT", dep_analyte = "DRUG"),
    "Analyte NONEXISTENT not found in nif object"
  )

  # Test missing dependent analyte
  expect_error(
    correlate_obs(test_nif, indep_analyte = "DRUG", dep_analyte = "NONEXISTENT"),
    "Analyte NONEXISTENT not found in nif object"
  )

  # Test NULL independent analyte
  expect_error(
    correlate_obs(test_nif, indep_analyte = NULL, dep_analyte = "DRUG"),
    "must not be NULL"
  )

  # Test NULL dependent analyte
  expect_error(
    correlate_obs(test_nif, indep_analyte = "DRUG", dep_analyte = NULL),
    "must not be NULL"
  )
})


test_that("correlate_obs validates window parameter", {
  # Create valid test nif object with both analytes
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~EVID, ~ANALYTE, ~DTC, ~REF,
      1,     0,   0,     1,   "DRUG",   "2023-01-01T00:00:00", 1,
      1,     0,  10,     0,   "DRUG",   "2023-01-01T00:00:00", 2,
      1,     0,   5,     0,   "METAB",  "2023-01-01T00:00:00", 3
  ) %>%
    nif() %>%
    lubrify_dates()

  # Test invalid window (non-numeric)
  expect_error(
    correlate_obs(test_nif, "DRUG", "METAB", window = "invalid"),
    "must be a numeric value"
  )

  # Test NULL window
  expect_error(
    correlate_obs(test_nif, "DRUG", "METAB", window = NULL),
    "must not be NULL"
  )
})


test_that("correlate_obs validates time_field parameter", {
  # Create valid test nif object
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~EVID, ~ANALYTE, ~DTC, ~REF,
      1,     0,   0,     1,   "DRUG",   "2023-01-01T00:00:00", 1,
      1,     0,  10,     0,   "DRUG",   "2023-01-01T00:00:00", 2,
      1,     0,   5,     0,   "METAB",  "2023-01-01T00:00:00", 3
  ) %>%
    nif() %>%
    lubrify_dates()

  # Test invalid time_field value
  expect_error(
    correlate_obs(test_nif, "DRUG", "METAB", time_field = "INVALID"),
    "time_field.*must be one of"
  )

  # Test missing TIME field
  test_nif_no_time <- test_nif %>% select(-TIME)
  expect_error(
    correlate_obs(test_nif_no_time, "DRUG", "METAB", time_field = "TIME"),
    "Time field 'TIME' not found in input"
  )

  # Test missing DTC field
  test_nif_no_dtc <- test_nif %>% select(-DTC)
  expect_error(
    correlate_obs(test_nif_no_dtc, "DRUG", "METAB", time_field = "DTC"),
    "Time field 'DTC' not found in input"
  )
})


test_that("correlate_obs basic functionality works with single dependent analyte", {
  # Create test nif object with two analytes at same time
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~EVID, ~ANALYTE, ~DTC, ~REF,
      1,     0,   0,     1,   "DRUG",   "2023-01-01T00:00:00", 1,
      1,     0,  10,     0,   "DRUG",   "2023-01-01T00:00:00", 2,
      1,     0,   5,     0,   "METAB",  "2023-01-01T00:00:00", 3
  ) %>%
    nif() %>%
    lubrify_dates()

  result <- correlate_obs(
    test_nif, indep_analyte = "DRUG", dep_analyte = "METAB")

  # Check structure
  expect_s3_class(result, "data.frame")
  expect_true("DRUG" %in% names(result))
  expect_true("METAB" %in% names(result))
  expect_true("METAB_TIME" %in% names(result))

  # Check that correct values are paired
  expect_equal(result$DRUG, 10)
  expect_equal(result$METAB, 5)
  expect_equal(result$METAB_TIME, 0)
})


test_that("correlate_obs works with multiple dependent analytes", {
  # Create test nif object with one independent and multiple dependent analytes
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~EVID, ~ANALYTE, ~DTC, ~REF,
      1,     0,   0,     1,   "DRUG",   "2023-01-01T00:00:00", 1,
      1,     0,  10,     0,   "DRUG",   "2023-01-01T00:00:00", 2,
      1,     0,   5,     0,   "METAB",  "2023-01-01T00:00:00", 3,
      1,     0,   3,     0,   "BIOMARKER", "2023-01-01T00:00:00", 4
  ) %>%
    nif() %>%
    lubrify_dates()

  result <- correlate_obs(
    test_nif, indep_analyte = "DRUG", dep_analyte = c("METAB", "BIOMARKER"))

  # Check structure
  expect_s3_class(result, "data.frame")
  expect_true("DRUG" %in% names(result))
  expect_true("METAB" %in% names(result))
  expect_true("BIOMARKER" %in% names(result))
  expect_true("METAB_TIME" %in% names(result))
  expect_true("BIOMARKER_TIME" %in% names(result))

  # Check values
  expect_equal(result$DRUG, 10)
  expect_equal(result$METAB, 5)
  expect_equal(result$BIOMARKER, 3)
})


test_that("correlate_obs matches observations within time window using TIME", {
  # Create test nif object with two analytes at slightly different times
  # TIME is in hours, so 5 minutes = 5/60 = 0.083 hours
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~EVID, ~ANALYTE, ~DTC, ~REF,
      1,     0,   0,     1,   "DRUG",   "2023-01-01T00:00:00", 1,
      1,     0,  10,     0,   "DRUG",   "2023-01-01T00:00:00", 2,
      1,     5/60,   5,     0,   "METAB",  "2023-01-01T00:05:00", 3  # 5 minutes later
  ) %>%
    nif() %>%
    lubrify_dates()

  # Default window is 10/60 = 10 minutes, so this should match
  # TIME difference is 5 minutes = 5/60 hours = 0.083 hours < 10/60 = 0.167 hours
  result <- correlate_obs(
    test_nif, indep_analyte = "DRUG", dep_analyte = "METAB", time_field = "TIME")

  expect_equal(nrow(result), 1)
  expect_equal(result$DRUG, 10)
  expect_equal(result$METAB, 5)

  # Test with smaller window that excludes the match
  # 5 minutes = 0.083 hours > 2/60 = 0.033 hours
  result_small <- correlate_obs(
    test_nif, indep_analyte = "DRUG", dep_analyte = "METAB",
    window = 2/60, time_field = "TIME")  # 2 minutes window

  # Should return empty data frame when no matches
  expect_equal(nrow(result_small), 0)
})


test_that("correlate_obs matches observations within time window using DTC", {
  # Create test nif object with two analytes at slightly different DTC times
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~EVID, ~ANALYTE, ~DTC, ~REF,
      1,     0,   0,     1,   "DRUG",   "2023-01-01T00:00:00", 1,
      1,     0,  10,     0,   "DRUG",   "2023-01-01T00:00:00", 2,
      1,     5,   5,     0,   "METAB",  "2023-01-01T00:05:00", 3  # 5 minutes later
  ) %>%
    nif() %>%
    lubrify_dates()

  # Default window is 10/60 = 10 minutes, so this should match
  result <- correlate_obs(
    test_nif, indep_analyte = "DRUG", dep_analyte = "METAB", time_field = "DTC")

  expect_equal(nrow(result), 1)
  expect_equal(result$DRUG, 10)
  expect_equal(result$METAB, 5)
})


test_that("correlate_obs handles multiple matches within window", {
  # Create test nif object with multiple dependent analyte observations within window
  # TIME is in hours, so 2 minutes = 2/60, 3 minutes = 3/60
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~EVID, ~ANALYTE, ~DTC, ~REF,
      1,     0,   0,     1,   "DRUG",   "2023-01-01T00:00:00", 1,
      1,     0,  10,     0,   "DRUG",   "2023-01-01T00:00:00", 2,
      1,     2/60,   5,     0,  "METAB",   "2023-01-01T00:02:00", 3,  # 2 minutes later
      1,     3/60,   7,     0,  "METAB",   "2023-01-01T00:03:00", 4   # 3 minutes later
  ) %>%
    nif() %>%
    lubrify_dates()

  # Default duplicate_function is mean, so should average 5 and 7 = 6
  result <- correlate_obs(
    test_nif, indep_analyte = "DRUG", dep_analyte = "METAB")

  expect_equal(nrow(result), 1)
  expect_equal(result$DRUG, 10)
  expect_equal(result$METAB, 6)  # Mean of 5 and 7
  expect_equal(result$METAB_TIME, (2/60 + 3/60)/2)  # Mean of 2/60 and 3/60
})


test_that("correlate_obs handles custom duplicate function", {
  # Create test nif object with multiple dependent analyte observations
  # TIME is in hours, so 2 minutes = 2/60, 3 minutes = 3/60
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~EVID, ~ANALYTE, ~DTC, ~REF,
      1,     0,   0,     1,   "DRUG",   "2023-01-01T00:00:00", 1,
      1,     0,  10,     0,   "DRUG",   "2023-01-01T00:00:00", 2,
      1,     2/60,   5,     0,   "METAB",  "2023-01-01T00:02:00", 3,
      1,     3/60,   7,     0,   "METAB",  "2023-01-01T00:03:00", 4
  ) %>%
    nif() %>%
    lubrify_dates()

  # Test with sum function
  result <- correlate_obs(
    test_nif, indep_analyte = "DRUG", dep_analyte = "METAB",
    duplicate_function = sum)

  expect_equal(nrow(result), 1)
  expect_equal(result$METAB, 12)  # Sum of 5 and 7
  expect_equal(result$METAB_TIME, 2/60 + 3/60)  # Sum of 2/60 and 3/60

  # Test with max function
  result2 <- correlate_obs(
    test_nif, indep_analyte = "DRUG", dep_analyte = "METAB",
    duplicate_function = max)

  expect_equal(result2$METAB, 7)  # Max of 5 and 7
  expect_equal(result2$METAB_TIME, 3/60)  # Max of 2/60 and 3/60

  # Test with min function
  result3 <- correlate_obs(
    test_nif, indep_analyte = "DRUG", dep_analyte = "METAB",
    duplicate_function = min)

  expect_equal(result3$METAB, 5)  # Min of 5 and 7
  expect_equal(result3$METAB_TIME, 2/60)  # Min of 2/60 and 3/60
})


test_that("correlate_obs handles no matches", {
  # Create test nif object with two analytes far apart in time
  # 2 hours = 120 minutes > default window of 10 minutes
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~EVID, ~ANALYTE, ~DTC, ~REF,
      1,     0,   0,     1,   "DRUG",   "2023-01-01T00:00:00", 1,
      1,     0,  10,     0,   "DRUG",   "2023-01-01T00:00:00", 2,
      1,   120,   5,     0,   "METAB",  "2023-01-01T02:00:00", 3  # 2 hours later
  ) %>%
    nif() %>%
    lubrify_dates()

  result <- correlate_obs(
    test_nif, indep_analyte = "DRUG", dep_analyte = "METAB")

  # Should return empty data frame when no matches
  expect_equal(nrow(result), 0)
})


test_that("correlate_obs filters NA and NaN values", {
  # Create test nif object with NA and NaN values
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~EVID, ~ANALYTE, ~DTC, ~REF,
      1,     0,   0,     1,   "DRUG",   "2023-01-01T00:00:00", 1,
      1,     0,  10,     0,   "DRUG",   "2023-01-01T00:00:00", 2,
      1,     0,  NA,     0,   "DRUG",   "2023-01-01T00:00:00", 3,  # NA value
      1,     0, NaN,     0,   "DRUG",   "2023-01-01T00:00:00", 4, # NaN value
      1,     0,   5,     0,   "METAB",  "2023-01-01T00:00:00", 5
  ) %>%
    nif() %>%
    lubrify_dates()

  result <- correlate_obs(
    test_nif, indep_analyte = "DRUG", dep_analyte = "METAB")

  # Should only have one row with valid DRUG value (10)
  expect_equal(nrow(result), 1)
  expect_equal(result$DRUG, 10)
  expect_equal(result$METAB, 5)
})


test_that("correlate_obs handles multiple subjects", {
  # Create test nif object with multiple subjects
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~EVID, ~ANALYTE, ~DTC, ~REF,
      1,     0,   0,     1,   "DRUG",   "2023-01-01T00:00:00", 1,
      1,     0,  10,     0,   "DRUG",   "2023-01-01T00:00:00", 2,
      1,     0,   5,     0,   "METAB",  "2023-01-01T00:00:00", 3,
      2,     0,   0,     1,   "DRUG",   "2023-01-01T00:00:00", 4,
      2,     0,  15,     0,   "DRUG",   "2023-01-01T00:00:00", 5,
      2,     0,   8,     0,   "METAB",  "2023-01-01T00:00:00", 6
  ) %>%
    nif() %>%
    lubrify_dates()

  result <- correlate_obs(
    test_nif, indep_analyte = "DRUG", dep_analyte = "METAB")

  # Should have 2 rows (one per subject)
  expect_equal(nrow(result), 2)

  # Check subject 1
  result1 <- result[result$ID == 1, ]
  expect_equal(result1$DRUG, 10)
  expect_equal(result1$METAB, 5)

  # Check subject 2
  result2 <- result[result$ID == 2, ]
  expect_equal(result2$DRUG, 15)
  expect_equal(result2$METAB, 8)
})


test_that("correlate_obs handles multiple independent analyte observations", {
  # Create test nif object with multiple independent analyte observations
  # Use a smaller window so each DRUG observation only matches its corresponding METAB
  # TIME is in hours, so 2 minutes = 2/60
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~EVID, ~ANALYTE, ~DTC, ~REF,
      1,     0,   0,     1,   "DRUG",   "2023-01-01T00:00:00", 1,
      1,     0,  10,     0,   "DRUG",   "2023-01-01T00:00:00", 2,
      1,     2/60,  20,     0,   "DRUG",   "2023-01-01T00:02:00", 3,
      1,     0,   5,     0,  "METAB",   "2023-01-01T00:00:00", 4,
      1,     2/60,  15,     0,  "METAB",   "2023-01-01T00:02:00", 5
  ) %>%
    nif() %>%
    lubrify_dates()

  # Use a small window (1 minute) so each matches only its corresponding observation
  result <- correlate_obs(
    test_nif, indep_analyte = "DRUG", dep_analyte = "METAB",
    window = 1/60)

  # Should have 2 rows (one per independent observation that has a match)
  expect_equal(nrow(result), 2)

  # Check results - sort by TIME to ensure consistent ordering
  result <- result[order(result$TIME), ]

  # First match should be at TIME 0
  expect_equal(result$DRUG[1], 10)
  expect_equal(result$METAB[1], 5)
  expect_equal(result$TIME[1], 0)

  # Second match should be at TIME 2/60
  expect_equal(result$DRUG[2], 20)
  expect_equal(result$METAB[2], 15)
  expect_equal(result$TIME[2], 2/60)
})


test_that("correlate_obs only matches observations from same subject", {
  # Create test nif object where times overlap but subjects differ
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~EVID, ~ANALYTE, ~DTC, ~REF,
      1,     0,   0,     1,   "DRUG",   "2023-01-01T00:00:00", 1,
      1,     0,  10,     0,   "DRUG",   "2023-01-01T00:00:00", 2,
      2,     0,   5,     0,  "METAB",   "2023-01-01T00:00:00", 3  # Different subject
  ) %>%
    nif() %>%
    lubrify_dates()

  # Function matches by ID, so different subjects shouldn't match
  result <- correlate_obs(
    test_nif, indep_analyte = "DRUG", dep_analyte = "METAB")

  # Should return empty data frame when no matches
  expect_equal(nrow(result), 0)
})


test_that("correlate_obs handles observations only with EVID == 0", {
  # Create test nif object with both EVID == 0 and EVID == 1
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~EVID, ~ANALYTE, ~DTC, ~REF,
    1,     0,   0,     1,   "DRUG",   "2023-01-01T00:00:00", 1,  # EVID == 1 (dose)
    1,     0,  10,     0,   "DRUG",   "2023-01-01T00:00:00", 2,  # EVID == 0 (observation)
    1,     0,   5,     0,   "METAB",  "2023-01-01T00:00:00", 3   # EVID == 0 (observation)
  ) %>%
    nif() %>%
    lubrify_dates()

  result <- correlate_obs(
    test_nif, indep_analyte = "DRUG", dep_analyte = "METAB")

  # Should only match observations (EVID == 0), not doses (EVID == 1)
  expect_equal(nrow(result), 1)
  expect_equal(result$DRUG, 10)  # Should use observation, not dose
  expect_equal(result$METAB, 5)
})


test_that("correlate_obs handles multiple dependent analytes with different matches", {
  # Create test nif object where one dependent analyte matches but another doesn't
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~EVID, ~ANALYTE, ~DTC, ~REF,
      1,     0,   0,     1,   "DRUG",   "2023-01-01T00:00:00", 1,
      1,     0,  10,     0,   "DRUG",   "2023-01-01T00:00:00", 2,
      1,     0,   5,     0,   "METAB",  "2023-01-01T00:00:00", 3,  # Matches
      1,   120,   3,     0,   "BIOMARKER", "2023-01-01T02:00:00", 4  # Doesn't match (too far)
  ) %>%
    nif() %>%
    lubrify_dates()

  result <- correlate_obs(
    test_nif, indep_analyte = "DRUG", dep_analyte = c("METAB", "BIOMARKER"))

  # Should have 1 row with METAB but BIOMARKER won't be present (no match)
  expect_equal(nrow(result), 1)
  expect_equal(result$DRUG, 10)
  expect_equal(result$METAB, 5)
  # BIOMARKER won't be present since there's no match
  expect_false("BIOMARKER" %in% names(result))
})


test_that("correlate_obs handles very small time window", {
  # Create test nif object with observations very close in time
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~EVID, ~ANALYTE, ~DTC, ~REF,
      1,     0,   0,     1,   "DRUG",   "2023-01-01T00:00:00", 1,
      1,     0,  10,     0,   "DRUG",   "2023-01-01T00:00:00", 2,
      1,     0.000278,   5,     0,   "METAB",  "2023-01-01T00:00:01", 3  # 1 second later (0.000278 hours)
  ) %>%
    nif() %>%
    lubrify_dates()

  # With 1 minute window (1/60 hours = 0.01667 hours), should match
  # 1 second = 1/3600 hours = 0.000278 hours < 0.01667 hours
  result <- correlate_obs(
    test_nif, indep_analyte = "DRUG", dep_analyte = "METAB", window = 1/60)

  expect_equal(nrow(result), 1)

  # With 0.5 second window (0.5/3600 hours = 0.000139 hours), should not match
  # 1 second = 0.000278 hours > 0.000139 hours
  result_small <- correlate_obs(
    test_nif, indep_analyte = "DRUG", dep_analyte = "METAB",
    window = 0.5/3600)

  expect_equal(nrow(result_small), 0)
})


test_that("correlate_obs handles backward time differences", {
  # Create test nif object where dependent is before independent
  # TIME is in hours, so 5 minutes = 5/60
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~EVID, ~ANALYTE, ~DTC, ~REF,
      1,     5/60,   0,     1,   "DRUG",   "2023-01-01T00:05:00", 1,
      1,     5/60,  10,     0,   "DRUG",   "2023-01-01T00:05:00", 2,
      1,     0,   5,     0,  "METAB",   "2023-01-01T00:00:00", 3  # 5 minutes before
  ) %>%
    nif() %>%
    lubrify_dates()

  # Default window is 10 minutes, so should match (abs(5 minutes) < 10 minutes)
  result <- correlate_obs(
    test_nif, indep_analyte = "DRUG", dep_analyte = "METAB")

  expect_equal(nrow(result), 1)
  expect_equal(result$DRUG, 10)
  expect_equal(result$METAB, 5)
})


test_that("correlate_obs handles TIME vs DTC field differences", {
  # Create test nif where TIME and DTC give different matching results
  # TIME is in hours, so 5 minutes = 5/60
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~EVID, ~ANALYTE, ~DTC, ~REF,
      1,     0,   0,     1,   "DRUG",   "2023-01-01T00:00:00", 1,
      1,     0,  10,     0,   "DRUG",   "2023-01-01T00:00:00", 2,
      1,     5/60,   5,     0,   "METAB",  "2023-01-01T00:05:00", 3  # 5 minutes later
  ) %>%
    nif() %>%
    lubrify_dates()

  # Test with TIME field - should match (5 minutes < 10 minutes window)
  result_time <- correlate_obs(
    test_nif, indep_analyte = "DRUG", dep_analyte = "METAB",
    time_field = "TIME", window = 10/60)

  expect_equal(nrow(result_time), 1)
  expect_equal(result_time$DRUG, 10)
  expect_equal(result_time$METAB, 5)

  # Test with DTC field - should also match (5 minutes < 10 minutes window)
  result_dtc <- correlate_obs(
    test_nif, indep_analyte = "DRUG", dep_analyte = "METAB",
    time_field = "DTC", window = 10/60)

  expect_equal(nrow(result_dtc), 1)
  expect_equal(result_dtc$DRUG, 10)
  expect_equal(result_dtc$METAB, 5)
})


test_that("correlate_obs handles multiple independent observations with multiple dependent analytes", {
  # Complex scenario: multiple independent observations, each matching different dependent analytes
  # TIME is in hours, so 2 minutes = 2/60
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~EVID, ~ANALYTE, ~DTC, ~REF,
      1,     0,   0,     1,   "DRUG",   "2023-01-01T00:00:00", 1,
      1,     0,  10,     0,   "DRUG",   "2023-01-01T00:00:00", 2,
      1,     2/60,  20,     0,   "DRUG",   "2023-01-01T00:02:00", 3,
      1,     0,   5,     0,   "METAB",  "2023-01-01T00:00:00", 4,
      1,     2/60,  15,     0,   "METAB",  "2023-01-01T00:02:00", 5,
      1,     0,   7,     0,   "BIOMARKER", "2023-01-01T00:00:00", 6,
      1,     2/60,  12,     0,   "BIOMARKER", "2023-01-01T00:02:00", 7
  ) %>%
    nif() %>%
    lubrify_dates()

  # Use small window so each independent observation matches only corresponding dependent observations
  result <- correlate_obs(
    test_nif, indep_analyte = "DRUG", dep_analyte = c("METAB", "BIOMARKER"),
    window = 1/60)

  # Should have 2 rows (one per independent observation)
  expect_equal(nrow(result), 2)

  # Sort by TIME
  result <- result[order(result$TIME), ]

  # First match at TIME 0
  expect_equal(result$DRUG[1], 10)
  expect_equal(result$METAB[1], 5)
  expect_equal(result$BIOMARKER[1], 7)

  # Second match at TIME 2
  expect_equal(result$DRUG[2], 20)
  expect_equal(result$METAB[2], 15)
  expect_equal(result$BIOMARKER[2], 12)
})

