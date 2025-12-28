test_that("add_time_deviation works with basic input", {
  # Create a simple test data frame with scheduled observations
  test_data <- tibble::tribble(
    ~USUBJID, ~DTC, ~EVID, ~PARENT, ~ANALYTE, ~TAD, ~NTIME, ~DV,
    "SUBJ-001", "2020-01-01 08:00:00", 1, "DRUG", "DRUG", 0, 0, NA,
    "SUBJ-001", "2020-01-01 10:00:00", 0, "DRUG", "DRUG", 2, 2, 10,
    "SUBJ-001", "2020-01-01 12:00:00", 0, "DRUG", "DRUG", 4, 4, 20,
    "SUBJ-001", "2020-01-01 16:00:00", 0, "DRUG", "DRUG", 8, 8, 30
  ) %>%
    mutate(DTC = lubridate::as_datetime(DTC)) %>%
    nif()

  result <- add_time_deviation(test_data)

  # Check that TIME_DEV was added
  expect_true("TIME_DEV" %in% names(result))

  # Check TIME_DEV values (should be 0 for on-time observations)
  expect_equal(result$TIME_DEV[result$EVID == 0], c(0, 0, 0))
})


test_that("add_time_deviation calculates deviations for early/late observations", {
  # Create test data with observations that are early or late
  test_data <- tibble::tribble(
    ~USUBJID, ~DTC, ~EVID, ~PARENT, ~ANALYTE, ~TAD, ~NTIME, ~DV,
    "SUBJ-001", "2020-01-01 08:00:00", 1, "DRUG", "DRUG", 0, 0, NA,
    "SUBJ-001", "2020-01-01 09:30:00", 0, "DRUG", "DRUG", 1.5, 2, 10,  # 30 min early
    "SUBJ-001", "2020-01-01 12:30:00", 0, "DRUG", "DRUG", 4.5, 4, 20,  # 30 min late
    "SUBJ-001", "2020-01-01 15:00:00", 0, "DRUG", "DRUG", 7, 8, 30      # 1 hour early
  ) %>%
    mutate(DTC = lubridate::as_datetime(DTC)) %>%
    nif()

  result <- add_time_deviation(test_data)

  # Check TIME_DEV values (TAD - NTIME)
  expect_equal(result$TIME_DEV[result$EVID == 0], c(-0.5, 0.5, -1))
})


test_that("add_time_deviation handles pre-dose observations (NTIME == 0)", {
  # Create test data with pre-dose observation
  test_data <- tibble::tribble(
    ~USUBJID, ~DTC, ~EVID, ~PARENT, ~ANALYTE, ~TAD, ~NTIME, ~DV,
    "SUBJ-001", "2020-01-01 07:00:00", 0, "DRUG", "DRUG", -1, 0, 5,   # Pre-dose, 1 hour before
    "SUBJ-001", "2020-01-01 08:00:00", 1, "DRUG", "DRUG", 0, 0, NA,
    "SUBJ-001", "2020-01-01 10:00:00", 0, "DRUG", "DRUG", 2, 2, 10
  ) %>%
    mutate(DTC = lubridate::as_datetime(DTC)) %>%
    nif()

  result <- add_time_deviation(test_data)

  # Pre-dose observation should use TTND (time to next dose)
  # DTC difference: 07:00 to 08:00 = -1 hour
  expect_equal(result$TIME_DEV[result$NTIME == 0 & result$EVID == 0], -1)

  # Regular observation should use TAD - NTIME
  expect_equal(result$TIME_DEV[result$NTIME == 2], 0)
})


test_that("add_time_deviation handles multiple administrations", {
  # Create test data with multiple doses
  test_data <- tibble::tribble(
    ~USUBJID, ~DTC, ~EVID, ~PARENT, ~ANALYTE, ~TAD, ~NTIME, ~DV,
    "SUBJ-001", "2020-01-01 08:00:00", 1, "DRUG", "DRUG", 0, 0, NA,
    "SUBJ-001", "2020-01-01 10:00:00", 0, "DRUG", "DRUG", 2, 2, 10,
    "SUBJ-001", "2020-01-01 24:00:00", 1, "DRUG", "DRUG", 0, 0, NA,  # Second dose
    "SUBJ-001", "2020-01-02 02:00:00", 0, "DRUG", "DRUG", 2, 2, 20
  ) %>%
    mutate(DTC = lubridate::as_datetime(DTC)) %>%
    nif()

  result <- add_time_deviation(test_data)

  # Check that TIME_DEV is calculated correctly for both dosing intervals
  expect_equal(result$TIME_DEV[result$EVID == 0], c(0, 0))
})


test_that("add_time_deviation handles multiple subjects", {
  # Create test data with multiple subjects
  test_data <- tibble::tribble(
    ~USUBJID, ~DTC, ~EVID, ~PARENT, ~ANALYTE, ~TAD, ~NTIME, ~DV,
    "SUBJ-001", "2020-01-01 08:00:00", 1, "DRUG", "DRUG", 0, 0, NA,
    "SUBJ-001", "2020-01-01 10:00:00", 0, "DRUG", "DRUG", 2, 2, 10,
    "SUBJ-002", "2020-01-01 08:00:00", 1, "DRUG", "DRUG", 0, 0, NA,
    "SUBJ-002", "2020-01-01 11:00:00", 0, "DRUG", "DRUG", 3, 2, 20  # 1 hour late
  ) %>%
    mutate(DTC = lubridate::as_datetime(DTC)) %>%
    nif()

  result <- add_time_deviation(test_data)

  # Check TIME_DEV for each subject
  expect_equal(result$TIME_DEV[result$USUBJID == "SUBJ-001" & result$EVID == 0], 0)
  expect_equal(result$TIME_DEV[result$USUBJID == "SUBJ-002" & result$EVID == 0], 1)
})


test_that("add_time_deviation handles multiple parent compounds", {
  # Create test data with multiple parent compounds
  test_data <- tibble::tribble(
    ~USUBJID, ~DTC, ~EVID, ~PARENT, ~ANALYTE, ~TAD, ~NTIME, ~DV,
    "SUBJ-001", "2020-01-01 08:00:00", 1, "DRUG1", "DRUG1", 0, 0, NA,
    "SUBJ-001", "2020-01-01 10:00:00", 0, "DRUG1", "DRUG1", 2, 2, 10,
    "SUBJ-001", "2020-01-01 08:00:00", 1, "DRUG2", "DRUG2", 0, 0, NA,
    "SUBJ-001", "2020-01-01 10:00:00", 0, "DRUG2", "DRUG2", 2, 2, 20
  ) %>%
    mutate(DTC = lubridate::as_datetime(DTC)) %>%
    nif()

  result <- add_time_deviation(test_data)

  # Check TIME_DEV for each parent
  expect_equal(result$TIME_DEV[result$PARENT == "DRUG1" & result$EVID == 0], 0)
  expect_equal(result$TIME_DEV[result$PARENT == "DRUG2" & result$EVID == 0], 0)
})


test_that("add_time_deviation handles observations without next administration", {
  # Create test data where last observation has no next administration
  test_data <- tibble::tribble(
    ~USUBJID, ~DTC, ~EVID, ~PARENT, ~ANALYTE, ~TAD, ~NTIME, ~DV,
    "SUBJ-001", "2020-01-01 08:00:00", 1, "DRUG", "DRUG", 0, 0, NA,
    "SUBJ-001", "2020-01-01 10:00:00", 0, "DRUG", "DRUG", 2, 2, 10,
    "SUBJ-001", "2020-01-01 12:00:00", 0, "DRUG", "DRUG", -2, 0, 5   # Pre-dose, but no next admin
  ) %>%
    mutate(DTC = lubridate::as_datetime(DTC)) %>%
    nif()

  result <- add_time_deviation(test_data)

  # Pre-dose observation without next admin should have NA for TTND
  pre_dose_row <- result[result$NTIME == 0 & result$EVID == 0, ]
  expect_true(is.na(pre_dose_row$TIME_DEV))
})


test_that("add_time_deviation handles NA values in TAD", {
  # Create test data with NA in TAD
  test_data <- tibble::tribble(
    ~USUBJID, ~DTC, ~EVID, ~PARENT, ~ANALYTE, ~TAD, ~NTIME, ~DV,
    "SUBJ-001", "2020-01-01 08:00:00", 1, "DRUG", "DRUG", 0, 0, NA,
    "SUBJ-001", "2020-01-01 10:00:00", 0, "DRUG", "DRUG", NA, 2, 10,
    "SUBJ-001", "2020-01-01 12:00:00", 0, "DRUG", "DRUG", 4, 4, 20
  ) %>%
    mutate(DTC = lubridate::as_datetime(DTC)) %>%
    nif()

  result <- add_time_deviation(test_data)

  # TIME_DEV should be NA when TAD is NA
  na_tad_row <- result[is.na(result$TAD) & result$EVID == 0, ]
  expect_true(is.na(na_tad_row$TIME_DEV))
})


test_that("add_time_deviation handles NA values in NTIME", {
  # Create test data with NA in NTIME (but not 0)
  test_data <- tibble::tribble(
    ~USUBJID, ~DTC, ~EVID, ~PARENT, ~ANALYTE, ~TAD, ~NTIME, ~DV,
    "SUBJ-001", "2020-01-01 08:00:00", 1, "DRUG", "DRUG", 0, 0, NA,
    "SUBJ-001", "2020-01-01 10:00:00", 0, "DRUG", "DRUG", 2, NA, 10,
    "SUBJ-001", "2020-01-01 12:00:00", 0, "DRUG", "DRUG", 4, 4, 20
  ) %>%
    mutate(DTC = lubridate::as_datetime(DTC)) %>%
    nif()

  expect_message(
    result <- add_time_deviation(test_data, silent = F),
    "Missing NTIME information in 1"
  )

  # TIME_DEV should be NA when NTIME is NA (TAD - NA = NA)
  na_ntime_row <- result[is.na(result$NTIME) & result$EVID == 0, ]
  expect_true(is.na(na_ntime_row$TIME_DEV))
})


test_that("add_time_deviation rounds TIME_DEV to 3 decimal places", {
  # Create test data with precise timing
  test_data <- tibble::tribble(
    ~USUBJID, ~DTC, ~EVID, ~PARENT, ~ANALYTE, ~TAD, ~NTIME, ~DV,
    "SUBJ-001", "2020-01-01 08:00:00", 1, "DRUG", "DRUG", 0, 0, NA,
    "SUBJ-001", "2020-01-01 10:00:00.123", 0, "DRUG", "DRUG", 2.0003417, 2, 10  # Very precise
  ) %>%
    mutate(DTC = lubridate::as_datetime(DTC)) %>%
    nif()

  result <- add_time_deviation(test_data)

  # TIME_DEV should be rounded to 3 decimal places
  time_dev <- result$TIME_DEV[result$EVID == 0]
  expect_equal(round(time_dev, 3), time_dev)
})


test_that("add_time_deviation handles empty data frame", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DTC, ~EVID, ~PARENT, ~ANALYTE, ~TAD, ~NTIME, ~DV
  ) %>%
    mutate(DTC = lubridate::as_datetime(character(0))) %>%
    nif()

  result <- add_time_deviation(test_data)

  expect_equal(nrow(result), 0)
  expect_true("TIME_DEV" %in% names(result))
})


test_that("add_time_deviation handles missing TAD field", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DTC, ~EVID, ~PARENT, ~NTIME, ~DV,
    "SUBJ-001", "2020-01-01 08:00:00", 1, "DRUG", 0, NA
  ) %>%
    mutate(DTC = lubridate::as_datetime(DTC)) %>%
    nif()

  expect_error(
    add_time_deviation(test_data),
    "Missing.*field.*in nif object.*TAD"
  )
})


test_that("add_time_deviation handles missing NTIME field", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DTC, ~EVID, ~PARENT, ~TAD, ~DV,
    "SUBJ-001", "2020-01-01 08:00:00", 1, "DRUG", 0, NA
  ) %>%
    mutate(DTC = lubridate::as_datetime(DTC)) %>%
    nif()

  expect_error(
    add_time_deviation(test_data),
    "Missing.*field.*in nif object.*NTIME"
  )
})


test_that("add_time_deviation preserves original data columns", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DTC, ~EVID, ~PARENT, ~ANALYTE, ~TAD, ~NTIME, ~DV, ~EXTRA_COL,
    "SUBJ-001", "2020-01-01 08:00:00", 1, "DRUG", "DRUG", 0, 0, NA, "A",
    "SUBJ-001", "2020-01-01 10:00:00", 0, "DRUG", "DRUG", 2, 2, 10, "B"
  ) %>%
    mutate(DTC = lubridate::as_datetime(DTC)) %>%
    nif()

  result <- add_time_deviation(test_data)

  # Check that original columns are preserved
  expect_true(all(names(test_data) %in% names(result)))
  expect_equal(result$EXTRA_COL, test_data$EXTRA_COL)
})


test_that("add_time_deviation removes temporary columns", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DTC, ~EVID, ~PARENT, ~ANALYTE, ~TAD, ~NTIME, ~DV,
    "SUBJ-001", "2020-01-01 08:00:00", 1, "DRUG", "DRUG", 0, 0, NA,
    "SUBJ-001", "2020-01-01 10:00:00", 0, "DRUG", "DRUG", 2, 2, 10
  ) %>%
    mutate(DTC = lubridate::as_datetime(DTC)) %>%
    nif()

  result <- add_time_deviation(test_data)

  # Check that temporary columns are removed
  expect_false("TTND" %in% names(result))
  expect_false("next_admin" %in% names(result))
})


test_that("add_time_deviation returns a nif object", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DTC, ~EVID, ~PARENT, ~ANALYTE, ~TAD, ~NTIME, ~DV,
    "SUBJ-001", "2020-01-01 08:00:00", 1, "DRUG", "DRUG", 0, 0, NA,
    "SUBJ-001", "2020-01-01 10:00:00", 0, "DRUG", "DRUG", 2, 2, 10
  ) %>%
    mutate(DTC = lubridate::as_datetime(DTC)) %>%
    nif()

  result <- add_time_deviation(test_data)

  expect_s3_class(result, "nif")
})


test_that("add_time_deviation handles complex scenario with multiple pre-dose observations", {
  # Create test data with multiple pre-dose observations before different administrations
  test_data <- tibble::tribble(
    ~USUBJID, ~DTC, ~EVID, ~PARENT, ~ANALYTE, ~TAD, ~NTIME, ~DV,
    "SUBJ-001", "2020-01-01 07:00:00", 0, "DRUG", "DRUG", -1, 0, 5,   # Pre-dose before first
    "SUBJ-001", "2020-01-01 08:00:00", 1, "DRUG", "DRUG", 0, 0, NA,
    "SUBJ-001", "2020-01-01 10:00:00", 0, "DRUG", "DRUG", 2, 2, 10,
    "SUBJ-001", "2020-01-01 23:00:00", 0, "DRUG", "DRUG", -1, 0, 6,   # Pre-dose before second
    "SUBJ-001", "2020-01-02 00:00:00", 1, "DRUG", "DRUG", 0, 0, NA,   # Second dose
    "SUBJ-001", "2020-01-02 02:00:00", 0, "DRUG", "DRUG", 2, 2, 20
  ) %>%
    mutate(DTC = lubridate::as_datetime(DTC)) %>%
    nif()

  result <- add_time_deviation(test_data)

  # First pre-dose: 1 hour before first dose
  expect_equal(result$TIME_DEV[result$DTC == lubridate::as_datetime("2020-01-01 07:00:00")], -1)

  # Second pre-dose: 1 hour before second dose
  expect_equal(result$TIME_DEV[result$DTC == lubridate::as_datetime("2020-01-01 23:00:00")], -1)

  # Regular observations should be on time
  expect_equal(result$TIME_DEV[result$NTIME == 2], c(0, 0))
})


test_that("add_time_deviation handles observations exactly at scheduled time", {
  # Create test data with observations exactly on time
  test_data <- tibble::tribble(
    ~USUBJID, ~DTC, ~EVID, ~PARENT, ~ANALYTE, ~TAD, ~NTIME, ~DV,
    "SUBJ-001", "2020-01-01 08:00:00", 1, "DRUG", "DRUG", 0, 0, NA,
    "SUBJ-001", "2020-01-01 10:00:00", 0, "DRUG", "DRUG", 2, 2, 10,
    "SUBJ-001", "2020-01-01 12:00:00", 0, "DRUG", "DRUG", 4, 4, 20,
    "SUBJ-001", "2020-01-01 16:00:00", 0, "DRUG", "DRUG", 8, 8, 30
  ) %>%
    mutate(DTC = lubridate::as_datetime(DTC)) %>%
    nif()

  result <- add_time_deviation(test_data)

  # All observations should have TIME_DEV = 0
  expect_equal(result$TIME_DEV[result$EVID == 0], rep(0, 3))
})


test_that("add_time_deviation handles very large time deviations", {
  # Create test data with very early/late observations
  test_data <- tibble::tribble(
    ~USUBJID, ~DTC, ~EVID, ~PARENT, ~ANALYTE, ~TAD, ~NTIME, ~DV,
    "SUBJ-001", "2020-01-01 08:00:00", 1, "DRUG", "DRUG", 0, 0, NA,
    "SUBJ-001", "2020-01-01 05:00:00", 0, "DRUG", "DRUG", -3, 2, 10,  # 5 hours early
    "SUBJ-001", "2020-01-01 20:00:00", 0, "DRUG", "DRUG", 12, 4, 20   # 8 hours late
  ) %>%
    mutate(DTC = lubridate::as_datetime(DTC)) %>%
    nif()

  result <- add_time_deviation(test_data)

  # Check large deviations
  expect_equal(result$TIME_DEV[result$TAD == -3], -5)  # -3 - 2 = -5
  expect_equal(result$TIME_DEV[result$TAD == 12], 8)   # 12 - 4 = 8
})


test_that("add_time_deviation handles negative NTIME values", {
  # Create test data with negative NTIME (if such exists)
  test_data <- tibble::tribble(
    ~USUBJID, ~DTC, ~EVID, ~PARENT, ~ANALYTE, ~TAD, ~NTIME, ~DV,
    "SUBJ-001", "2020-01-01 08:00:00", 1, "DRUG", "DRUG", 0, 0, NA,
    "SUBJ-001", "2020-01-01 10:00:00", 0, "DRUG", "DRUG", 2, -1, 10  # Negative NTIME
  ) %>%
    mutate(DTC = lubridate::as_datetime(DTC)) %>%
    nif()

  result <- add_time_deviation(test_data)

  # Should calculate TAD - NTIME (not use TTND since NTIME != 0)
  expect_equal(result$TIME_DEV[result$NTIME == -1], 3)  # 2 - (-1) = 3
})

