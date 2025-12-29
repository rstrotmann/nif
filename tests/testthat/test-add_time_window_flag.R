test_that("add_time_window_flag works with basic input", {
  # Create test data with observations
  test_data <- tibble::tribble(
    ~USUBJID, ~DTC, ~EVID, ~PARENT, ~ANALYTE, ~TAD, ~NTIME, ~DV,
    "SUBJ-001", "2020-01-01 08:00:00", 1, "DRUG", "DRUG", 0, 0, NA,
    "SUBJ-001", "2020-01-01 10:00:00", 0, "DRUG", "DRUG", 2, 2, 10,
    "SUBJ-001", "2020-01-01 12:00:00", 0, "DRUG", "DRUG", 4, 4, 20
  ) %>%
    mutate(DTC = lubridate::as_datetime(DTC)) %>%
    nif()

  # Create window definition (in minutes)
  window <- tibble::tribble(
    ~NTIME, ~BEFORE, ~AFTER,
    2, 30, 30,  # 30 min before/after for NTIME=2
    4, 30, 30   # 30 min before/after for NTIME=4
  )

  result <- add_time_window_flag(test_data, window, silent = TRUE)

  # Check that TIME_DEV, EXCL, EXCL_REASON were added
  expect_true("TIME_DEV" %in% names(result))
  expect_true("EXCL" %in% names(result))
  expect_true("EXCL_REASON" %in% names(result))

  # On-time observations should not be excluded
  expect_false(any(result$EXCL[result$EVID == 0]))
})


test_that("add_time_window_flag flags early observations", {
  # Create test data with early observation
  test_data <- tibble::tribble(
    ~USUBJID, ~DTC, ~EVID, ~PARENT, ~ANALYTE, ~TAD, ~NTIME, ~DV,
    "SUBJ-001", "2020-01-01 08:00:00", 1, "DRUG", "DRUG", 0, 0, NA,
    "SUBJ-001", "2020-01-01 09:00:00", 0, "DRUG", "DRUG", 1, 2, 10  # 1 hour early
  ) %>%
    mutate(DTC = lubridate::as_datetime(DTC)) %>%
    nif()

  # Window: 30 minutes before/after
  window <- tibble::tribble(
    ~NTIME, ~BEFORE, ~AFTER,
    2, 30, 30
  )

  result <- add_time_window_flag(test_data, window, silent = TRUE)

  # Early observation should be flagged
  expect_true(result$EXCL[result$NTIME == 2])
  expect_equal(result$EXCL_REASON[result$NTIME == 2], "time window violation")
})


test_that("add_time_window_flag flags late observations", {
  # Create test data with late observation
  test_data <- tibble::tribble(
    ~USUBJID, ~DTC, ~EVID, ~PARENT, ~ANALYTE, ~TAD, ~NTIME, ~DV,
    "SUBJ-001", "2020-01-01 08:00:00", 1, "DRUG", "DRUG", 0, 0, NA,
    "SUBJ-001", "2020-01-01 11:00:00", 0, "DRUG", "DRUG", 3, 2, 10  # 1 hour late
  ) %>%
    mutate(DTC = lubridate::as_datetime(DTC)) %>%
    nif()

  # Window: 30 minutes before/after
  window <- tibble::tribble(
    ~NTIME, ~BEFORE, ~AFTER,
    2, 30, 30
  )

  result <- add_time_window_flag(test_data, window, silent = TRUE)

  # Late observation should be flagged
  expect_true(result$EXCL[result$NTIME == 2])
  expect_equal(result$EXCL_REASON[result$NTIME == 2], "time window violation")
})


test_that("add_time_window_flag respects inclusive boundaries", {
  # Create test data with observations exactly at boundaries
  test_data <- tibble::tribble(
    ~USUBJID, ~DTC, ~EVID, ~PARENT, ~ANALYTE, ~TAD, ~NTIME, ~DV,
    "SUBJ-001", "2020-01-01 08:00:00", 1, "DRUG", "DRUG", 0, 0, NA,
    "SUBJ-001", "2020-01-01 09:30:00", 0, "DRUG", "DRUG", 1.5, 2, 10,  # Exactly 30 min early (boundary)
    "SUBJ-001", "2020-01-01 10:30:00", 0, "DRUG", "DRUG", 2.5, 2, 20   # Exactly 30 min late (boundary)
  ) %>%
    mutate(DTC = lubridate::as_datetime(DTC)) %>%
    nif()

  # Window: 30 minutes before/after (inclusive)
  window <- tibble::tribble(
    ~NTIME, ~BEFORE, ~AFTER,
    2, 30, 30
  )

  result <- add_time_window_flag(test_data, window, silent = TRUE)

  # Boundary observations should NOT be excluded (inclusive boundaries)
  expect_false(any(result$EXCL[result$EVID == 0]))
})


test_that("add_time_window_flag handles window in hours when use_minutes=FALSE", {
  # Create test data
  test_data <- tibble::tribble(
    ~USUBJID, ~DTC, ~EVID, ~PARENT, ~ANALYTE, ~TAD, ~NTIME, ~DV,
    "SUBJ-001", "2020-01-01 08:00:00", 1, "DRUG", "DRUG", 0, 0, NA,
    "SUBJ-001", "2020-01-01 09:00:00", 0, "DRUG", "DRUG", 1, 2, 10  # 1 hour early
  ) %>%
    mutate(DTC = lubridate::as_datetime(DTC)) %>%
    nif()

  # Window in hours (not minutes)
  window <- tibble::tribble(
    ~NTIME, ~BEFORE, ~AFTER,
    2, 0.5, 0.5  # 0.5 hours = 30 minutes
  )

  result <- add_time_window_flag(test_data, window, use_minutes = FALSE,
                                 silent = T)

  # Should flag the early observation
  expect_true(result$EXCL[result$NTIME == 2])
})


test_that("add_time_window_flag handles multiple NTIME values", {
  # Create test data with multiple scheduled times
  test_data <- tibble::tribble(
    ~USUBJID, ~DTC, ~EVID, ~PARENT, ~ANALYTE, ~TAD, ~NTIME, ~DV,
    "SUBJ-001", "2020-01-01 08:00:00", 1, "DRUG", "DRUG", 0, 0, NA,
    "SUBJ-001", "2020-01-01 10:00:00", 0, "DRUG", "DRUG", 2, 2, 10,
    "SUBJ-001", "2020-01-01 12:00:00", 0, "DRUG", "DRUG", 4, 4, 20,
    "SUBJ-001", "2020-01-01 16:00:00", 0, "DRUG", "DRUG", 8, 8, 30
  ) %>%
    mutate(DTC = lubridate::as_datetime(DTC)) %>%
    nif()

  # Window with different tolerances for different NTIME values
  window <- tibble::tribble(
    ~NTIME, ~BEFORE, ~AFTER,
    2, 15, 15,  # 15 min for NTIME=2
    4, 30, 30,  # 30 min for NTIME=4
    8, 60, 60   # 60 min for NTIME=8
  )

  result <- add_time_window_flag(test_data, window, silent = TRUE)

  # All on-time observations should not be excluded
  expect_false(any(result$EXCL[result$EVID == 0]))
})


test_that("add_time_window_flag guesses analyte when not specified", {
  # Create test data
  test_data <- tibble::tribble(
    ~USUBJID, ~DTC, ~EVID, ~PARENT, ~ANALYTE, ~TAD, ~NTIME, ~DV,
    "SUBJ-001", "2020-01-01 08:00:00", 1, "DRUG", "DRUG", 0, 0, NA,
    "SUBJ-001", "2020-01-01 10:00:00", 0, "DRUG", "DRUG", 2, 2, 10
  ) %>%
    mutate(DTC = lubridate::as_datetime(DTC)) %>%
    nif()

  window <- tibble::tribble(
    ~NTIME, ~BEFORE, ~AFTER,
    2, 30, 30
  )

  # Should work without specifying analyte (will guess)
  result <- add_time_window_flag(test_data, window, analyte = NULL, silent = TRUE)

  expect_true("TIME_DEV" %in% names(result))
  expect_true("EXCL" %in% names(result))
})


test_that("add_time_window_flag handles specified analyte", {
  # Create test data with multiple analytes
  test_data <- tibble::tribble(
    ~USUBJID, ~DTC, ~EVID, ~PARENT, ~ANALYTE, ~TAD, ~NTIME, ~DV,
    "SUBJ-001", "2020-01-01 08:00:00", 1, "DRUG1", "DRUG1", 0, 0, NA,
    "SUBJ-001", "2020-01-01 10:00:00", 0, "DRUG1", "DRUG1", 2, 2, 10,
    "SUBJ-001", "2020-01-01 08:00:00", 1, "DRUG2", "DRUG2", 0, 0, NA,
    "SUBJ-001", "2020-01-01 10:00:00", 0, "DRUG2", "DRUG2", 2, 2, 20
  ) %>%
    mutate(DTC = lubridate::as_datetime(DTC)) %>%
    nif()

  window <- tibble::tribble(
    ~NTIME, ~BEFORE, ~AFTER,
    2, 30, 30
  )

  # Should work with specified analyte
  result <- add_time_window_flag(test_data, window, analyte = "DRUG1", silent = TRUE)

  expect_true("TIME_DEV" %in% names(result))
  expect_true("EXCL" %in% names(result))
})


test_that("add_time_window_flag handles missing window fields", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DTC, ~EVID, ~PARENT, ~ANALYTE, ~TAD, ~NTIME, ~DV,
    "SUBJ-001", "2020-01-01 08:00:00", 1, "DRUG", "DRUG", 0, 0, NA,
    "SUBJ-001", "2020-01-01 08:00:00", 0, "DRUG", "DRUG", 0, 0, NA
  ) %>%
    mutate(DTC = lubridate::as_datetime(DTC)) %>%
    nif()

  # Window missing BEFORE field
  window <- tibble::tribble(
    ~NTIME, ~AFTER,
    2, 30
  )

  expect_error(
    add_time_window_flag(test_data, window, silent = TRUE),
    "Missing.*field.*in 'window'.*BEFORE"
  )
})


test_that("add_time_window_flag handles non-numeric window fields", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DTC, ~EVID, ~PARENT, ~ANALYTE, ~TAD, ~NTIME, ~DV,
    "SUBJ-001", "2020-01-01 08:00:00", 1, "DRUG", "DRUG", 0, 0, NA,
    "SUBJ-001", "2020-01-01 08:00:00", 0, "DRUG", "DRUG", 0, 0, NA
  ) %>%
    mutate(DTC = lubridate::as_datetime(DTC)) %>%
    nif()

  # Window with character BEFORE field
  window <- tibble::tribble(
    ~NTIME, ~BEFORE, ~AFTER,
    2, "30", 30
  )

  expect_error(
    add_time_window_flag(test_data, window, silent = TRUE),
    "All fields in 'window' must be numeric"
  )
})


test_that("add_time_window_flag handles negative window values", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DTC, ~EVID, ~PARENT, ~ANALYTE, ~TAD, ~NTIME, ~DV,
    "SUBJ-001", "2020-01-01 08:00:00", 1, "DRUG", "DRUG", 0, 0, NA,
    "SUBJ-001", "2020-01-01 08:00:00", 0, "DRUG", "DRUG", 0, 0, NA
  ) %>%
    mutate(DTC = lubridate::as_datetime(DTC)) %>%
    nif()

  # Window with negative BEFORE
  window <- tibble::tribble(
    ~NTIME, ~BEFORE, ~AFTER,
    2, -30, 30
  )

  expect_error(
    add_time_window_flag(test_data, window, silent = TRUE),
    "BEFORE and AFTER must be postivive numbers"
  )
})


test_that("add_time_window_flag handles invalid analyte", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DTC, ~EVID, ~PARENT, ~ANALYTE, ~TAD, ~NTIME, ~DV,
    "SUBJ-001", "2020-01-01 08:00:00", 1, "DRUG", "DRUG", 0, 0, NA
  ) %>%
    mutate(DTC = lubridate::as_datetime(DTC)) %>%
    nif()

  window <- tibble::tribble(
    ~NTIME, ~BEFORE, ~AFTER,
    2, 30, 30
  )

  expect_error(
    add_time_window_flag(test_data, window, analyte = "INVALID"),
    "analyte INVALID not found in nif object"
  )
})


test_that("add_time_window_flag handles non-data.frame window", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DTC, ~EVID, ~PARENT, ~ANALYTE, ~TAD, ~NTIME, ~DV,
    "SUBJ-001", "2020-01-01 08:00:00", 1, "DRUG", "DRUG", 0, 0, NA
  ) %>%
    mutate(DTC = lubridate::as_datetime(DTC)) %>%
    nif()

  expect_error(
    add_time_window_flag(test_data, window = "not a data frame"),
    "'window' must be a data frame"
  )
})


test_that("add_time_window_flag creates EXCL and EXCL_REASON if missing", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DTC, ~EVID, ~PARENT, ~ANALYTE, ~TAD, ~NTIME, ~DV,
    "SUBJ-001", "2020-01-01 08:00:00", 1, "DRUG", "DRUG", 0, 0, NA,
    "SUBJ-001", "2020-01-01 10:00:00", 0, "DRUG", "DRUG", 2, 2, 10
  ) %>%
    mutate(DTC = lubridate::as_datetime(DTC)) %>%
    nif()

  window <- tibble::tribble(
    ~NTIME, ~BEFORE, ~AFTER,
    2, 30, 30
  )

  result <- add_time_window_flag(test_data, window, silent = TRUE)

  # Should create EXCL and EXCL_REASON fields
  expect_true("EXCL" %in% names(result))
  expect_true("EXCL_REASON" %in% names(result))
  expect_false(any(result$EXCL[result$EVID == 0]))  # On-time, so not excluded
})


test_that("add_time_window_flag preserves existing EXCL values", {
  # Create test data with existing EXCL field
  test_data <- tibble::tribble(
    ~USUBJID, ~DTC, ~EVID, ~PARENT, ~ANALYTE, ~TAD, ~NTIME, ~DV, ~EXCL,
    "SUBJ-001", "2020-01-01 08:00:00", 1, "DRUG", "DRUG", 0, 0, NA, FALSE,
    "SUBJ-001", "2020-01-01 10:00:00", 0, "DRUG", "DRUG", 2, 2, 10, TRUE,  # Already excluded
    "SUBJ-001", "2020-01-01 12:00:00", 0, "DRUG", "DRUG", 4, 4, 20, FALSE
  ) %>%
    mutate(DTC = lubridate::as_datetime(DTC)) %>%
    nif()

  window <- tibble::tribble(
    ~NTIME, ~BEFORE, ~AFTER,
    2, 30, 30,
    4, 30, 30
  )

  result <- add_time_window_flag(test_data, window, silent = TRUE)

  # Existing EXCL=TRUE should be preserved
  expect_true(result$EXCL[result$NTIME == 2])
})


test_that("add_time_window_flag preserves existing EXCL_REASON values", {
  # Create test data with existing EXCL_REASON field
  test_data <- tibble::tribble(
    ~USUBJID, ~DTC, ~EVID, ~PARENT, ~ANALYTE, ~TAD, ~NTIME, ~DV, ~EXCL_REASON,
    "SUBJ-001", "2020-01-01 08:00:00", 1, "DRUG", "DRUG", 0, 0, NA, "",
    "SUBJ-001", "2020-01-01 10:00:00", 0, "DRUG", "DRUG", 2, 2, 10, "other reason",
    "SUBJ-001", "2020-01-01 12:00:00", 0, "DRUG", "DRUG", 4, 4, 20, ""
  ) %>%
    mutate(DTC = lubridate::as_datetime(DTC)) %>%
    nif()

  window <- tibble::tribble(
    ~NTIME, ~BEFORE, ~AFTER,
    2, 30, 30,
    4, 30, 30
  )

  result <- add_time_window_flag(test_data, window, silent = TRUE)

  # Existing EXCL_REASON should be preserved for non-violations
  expect_equal(result$EXCL_REASON[result$NTIME == 4], "")
})


test_that("add_time_window_flag updates EXCL_REASON for violations", {
  # Create test data with early observation
  test_data <- tibble::tribble(
    ~USUBJID, ~DTC, ~EVID, ~PARENT, ~ANALYTE, ~TAD, ~NTIME, ~DV,
    "SUBJ-001", "2020-01-01 08:00:00", 1, "DRUG", "DRUG", 0, 0, NA,
    "SUBJ-001", "2020-01-01 09:00:00", 0, "DRUG", "DRUG", 1, 2, 10  # 1 hour early
  ) %>%
    mutate(DTC = lubridate::as_datetime(DTC)) %>%
    nif()

  window <- tibble::tribble(
    ~NTIME, ~BEFORE, ~AFTER,
    2, 30, 30
  )

  result <- add_time_window_flag(test_data, window, silent = TRUE)

  # Should set EXCL_REASON to "time window violation"
  expect_equal(result$EXCL_REASON[result$NTIME == 2], "time window violation")
})


test_that("add_time_window_flag handles silent parameter", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DTC, ~EVID, ~PARENT, ~ANALYTE, ~TAD, ~NTIME, ~DV,
    "SUBJ-001", "2020-01-01 08:00:00", 1, "DRUG", "DRUG", 0, 0, NA,
    "SUBJ-001", "2020-01-01 10:00:00", 0, "DRUG", "DRUG", 2, 2, 10
  ) %>%
    mutate(DTC = lubridate::as_datetime(DTC)) %>%
    nif()

  window <- tibble::tribble(
    ~NTIME, ~BEFORE, ~AFTER,
    2, 30, 30
  )

  # Should work with silent = TRUE (suppresses messages)
  result <- add_time_window_flag(test_data, window, analyte = NULL, silent = TRUE)
  expect_true("TIME_DEV" %in% names(result))
})


# test_that("add_time_window_flag handles empty data frame", {
#   test_data <- tibble::tribble(
#     ~USUBJID, ~DTC, ~EVID, ~PARENT, ~ANALYTE, ~TAD, ~NTIME, ~DV
#   ) %>%
#     mutate(DTC = lubridate::as_datetime(character(0))) %>%
#     nif()
#
#   window <- tibble::tribble(
#     ~NTIME, ~BEFORE, ~AFTER,
#     2, 30, 30
#   )
#
#   result <- add_time_window_flag(test_data, window, silent = TRUE)
#
#   expect_equal(nrow(result), 0)
#   expect_true("TIME_DEV" %in% names(result))
#   expect_true("EXCL" %in% names(result))
#   expect_true("EXCL_REASON" %in% names(result))
# })


test_that("add_time_window_flag handles observations just outside boundary", {
  # Create test data with observations just outside the boundary
  test_data <- tibble::tribble(
    ~USUBJID, ~DTC, ~EVID, ~PARENT, ~ANALYTE, ~TAD, ~NTIME, ~DV,
    "SUBJ-001", "2020-01-01 08:00:00", 1, "DRUG", "DRUG", 0, 0, NA,
    "SUBJ-001", "2020-01-01 09:29:00", 0, "DRUG", "DRUG", 1.483, 2, 10,  # 31 min early (outside)
    "SUBJ-001", "2020-01-01 10:31:00", 0, "DRUG", "DRUG", 2.517, 2, 20   # 31 min late (outside)
  ) %>%
    mutate(DTC = lubridate::as_datetime(DTC)) %>%
    nif()

  # Window: 30 minutes before/after
  window <- tibble::tribble(
    ~NTIME, ~BEFORE, ~AFTER,
    2, 30, 30
  )

  result <- add_time_window_flag(test_data, window, silent = TRUE)

  # Observations just outside boundary should be excluded
  expect_true(all(result$EXCL[result$EVID == 0]))
})


test_that("add_time_window_flag handles multiple subjects", {
  # Create test data with multiple subjects
  test_data <- tibble::tribble(
    ~USUBJID, ~DTC, ~EVID, ~PARENT, ~ANALYTE, ~TAD, ~NTIME, ~DV,
    "SUBJ-001", "2020-01-01 08:00:00", 1, "DRUG", "DRUG", 0, 0, NA,
    "SUBJ-001", "2020-01-01 09:00:00", 0, "DRUG", "DRUG", 1, 2, 10,  # Early
    "SUBJ-002", "2020-01-01 08:00:00", 1, "DRUG", "DRUG", 0, 0, NA,
    "SUBJ-002", "2020-01-01 10:00:00", 0, "DRUG", "DRUG", 2, 2, 20   # On time
  ) %>%
    mutate(DTC = lubridate::as_datetime(DTC)) %>%
    nif()

  window <- tibble::tribble(
    ~NTIME, ~BEFORE, ~AFTER,
    2, 30, 30
  )

  result <- add_time_window_flag(test_data, window, silent = TRUE)

  # SUBJ-001 should be excluded, SUBJ-002 should not
  expect_true(result$EXCL[result$USUBJID == "SUBJ-001" & result$NTIME == 2])
  expect_false(result$EXCL[result$USUBJID == "SUBJ-002" & result$NTIME == 2])
})


test_that("add_time_window_flag handles NTIME not in window", {
  # Create test data with NTIME value not in window definition
  test_data <- tibble::tribble(
    ~USUBJID, ~DTC, ~EVID, ~PARENT, ~ANALYTE, ~TAD, ~NTIME, ~DV,
    "SUBJ-001", "2020-01-01 08:00:00", 1, "DRUG", "DRUG", 0, 0, NA,
    "SUBJ-001", "2020-01-01 10:00:00", 0, "DRUG", "DRUG", 2, 2, 10,
    "SUBJ-001", "2020-01-01 12:00:00", 0, "DRUG", "DRUG", 4, 4, 20  # NTIME=4 not in window
  ) %>%
    mutate(DTC = lubridate::as_datetime(DTC)) %>%
    nif()

  # Window only defines NTIME=2
  window <- tibble::tribble(
    ~NTIME, ~BEFORE, ~AFTER,
    2, 30, 30
  )

  result <- add_time_window_flag(test_data, window, silent = TRUE)

  # NTIME=4 should have NA for .BEFORE/.AFTER after join, so EXCL should remain FALSE
  expect_false(result$EXCL[result$NTIME == 4])
})


test_that("add_time_window_flag returns a nif object", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DTC, ~EVID, ~PARENT, ~ANALYTE, ~TAD, ~NTIME, ~DV,
    "SUBJ-001", "2020-01-01 08:00:00", 1, "DRUG", "DRUG", 0, 0, NA,
    "SUBJ-001", "2020-01-01 10:00:00", 0, "DRUG", "DRUG", 2, 2, 10
  ) %>%
    mutate(DTC = lubridate::as_datetime(DTC)) %>%
    nif()

  window <- tibble::tribble(
    ~NTIME, ~BEFORE, ~AFTER,
    2, 30, 30
  )

  result <- add_time_window_flag(test_data, window, silent = TRUE)

  expect_s3_class(result, "nif")
})


test_that("add_time_window_flag removes temporary columns", {
  test_data <- tibble::tribble(
    ~USUBJID, ~DTC, ~EVID, ~PARENT, ~ANALYTE, ~TAD, ~NTIME, ~DV,
    "SUBJ-001", "2020-01-01 08:00:00", 1, "DRUG", "DRUG", 0, 0, NA,
    "SUBJ-001", "2020-01-01 10:00:00", 0, "DRUG", "DRUG", 2, 2, 10
  ) %>%
    mutate(DTC = lubridate::as_datetime(DTC)) %>%
    nif()

  window <- tibble::tribble(
    ~NTIME, ~BEFORE, ~AFTER,
    2, 30, 30
  )

  result <- add_time_window_flag(test_data, window, silent = TRUE)

  # Temporary columns should be removed
  expect_false(".BEFORE" %in% names(result))
  expect_false(".AFTER" %in% names(result))
})


test_that("add_time_window_flag handles zero window values", {
  # Create test data
  test_data <- tibble::tribble(
    ~USUBJID, ~DTC, ~EVID, ~PARENT, ~ANALYTE, ~TAD, ~NTIME, ~DV,
    "SUBJ-001", "2020-01-01 08:00:00", 1, "DRUG", "DRUG", 0, 0, NA,
    "SUBJ-001", "2020-01-01 10:00:00", 0, "DRUG", "DRUG", 2, 2, 10,
    "SUBJ-001", "2020-01-01 10:01:00", 0, "DRUG", "DRUG", 2.017, 2, 20  # 1 min late
  ) %>%
    mutate(DTC = lubridate::as_datetime(DTC)) %>%
    nif()

  # Window with zero tolerance (exact timing required)
  window <- tibble::tribble(
    ~NTIME, ~BEFORE, ~AFTER,
    2, 0, 0
  )

  result <- add_time_window_flag(test_data, window, silent = TRUE)

  # Only exact timing should not be excluded
  expect_false(result$EXCL[result$TAD == 2])
  expect_true(result$EXCL[result$TAD == 2.017])  # 1 min late should be excluded
})

