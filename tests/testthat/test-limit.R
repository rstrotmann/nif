# Helper function to create a test nif object with DTC
create_test_nif_with_dtc <- function() {
  tibble::tribble(
    ~ID, ~STUDYID, ~TIME, ~DTC,                        ~EVID, ~AMT, ~CMT, ~DV, ~MDV,
    1,   "TEST01", 0,     "2024-01-01 08:00:00",      1,     100,  1,    NA,  1,  # Admin
    1,   "TEST01", 0,     "2024-01-01 08:00:00",      0,     0,    2,    0,   0,  # Obs
    1,   "TEST01", 4,     "2024-01-01 12:00:00",      0,     0,    2,    5.2, 0,  # Obs
    1,   "TEST01", 8,     "2024-01-01 16:00:00",      0,     0,    2,    8.5, 0,  # Obs (last)
    1,   "TEST01", 12,    "2024-01-01 20:00:00",      1,     100,  1,    NA,  1,  # Admin (after last obs)
    2,   "TEST01", 0,     "2024-01-01 09:00:00",      1,     150,  1,    NA,  1,  # Admin
    2,   "TEST01", 0,     "2024-01-01 09:00:00",      0,     0,    2,    0,   0,  # Obs
    2,   "TEST01", 6,     "2024-01-01 15:00:00",      0,     0,    2,    10.3, 0,  # Obs (last)
    2,   "TEST01", 9,     "2024-01-01 18:00:00",      1,     150,  1,    NA,  1,  # Admin (after last obs)
    2,   "TEST01", 12,    "2024-01-01 21:00:00",      1,     150,  1,    NA,  1   # Admin (after last obs)
  ) %>%
    dplyr::mutate(DTC = as.POSIXct(.data$DTC)) %>%
    nif(silent = TRUE)
}


test_that("limit validates inputs correctly", {
  # Create a valid test nif object
  test_nif <- create_test_nif_with_dtc()

  # Test invalid nif object
  expect_error(
    limit("not_nif"),
    "Input must be a nif object"
  )

  # Test invalid individual parameter (non-logical)
  expect_error(
    limit(test_nif, individual = "TRUE"),
    "individual must be a logical value"
  )

  # Test invalid individual parameter (numeric)
  expect_error(
    limit(test_nif, individual = 1),
    "individual must be a logical value"
  )

  # Test invalid keep_no_obs_sbs parameter (non-logical)
  expect_error(
    limit(test_nif, keep_no_obs_sbs = "FALSE"),
    "keep_no_obs_sbs must be a logical value"
  )

  # Test invalid keep_no_obs_sbs parameter (vector)
  expect_error(
    limit(test_nif, keep_no_obs_sbs = c(TRUE, FALSE)),
    "keep_no_obs_sbs must be a single value"
  )
})


test_that("limit with individual=TRUE filters per subject", {
  # Create test nif object
  test_nif <- create_test_nif_with_dtc()

  # Apply limit with individual=TRUE
  result <- limit(test_nif, individual = TRUE)

  # Verify it's still a nif object
  expect_s3_class(result, "nif")

  # Subject 1: Last obs at 16:00, should keep admin at 20:00? No, should filter it out
  # Subject 1 should have: admin at 08:00, obs at 08:00, 12:00, 16:00 (last obs)
  # Should NOT have: admin at 20:00 (after last obs)
  subject1_rows <- result %>% dplyr::filter(.data$ID == 1)
  expect_true(all(subject1_rows$DTC <= as.POSIXct("2024-01-01 16:00:00")))
  expect_false(any(subject1_rows$DTC == as.POSIXct("2024-01-01 20:00:00")))

  # Subject 2: Last obs at 15:00, should keep admin at 18:00? No, should filter it out
  # Subject 2 should have: admin at 09:00, obs at 09:00, 15:00 (last obs)
  # Should NOT have: admin at 18:00 or 21:00 (after last obs)
  subject2_rows <- result %>% dplyr::filter(.data$ID == 2)
  expect_true(all(subject2_rows$DTC <= as.POSIXct("2024-01-01 15:00:00")))
  expect_false(any(subject2_rows$DTC == as.POSIXct("2024-01-01 18:00:00")))
  expect_false(any(subject2_rows$DTC == as.POSIXct("2024-01-01 21:00:00")))

  # Verify all rows have DTC <= last observation for that subject
  for (id in unique(result$ID)) {
    subject_data <- result %>% dplyr::filter(.data$ID == id)
    last_obs_dtc <- max(subject_data$DTC[subject_data$EVID == 0], na.rm = TRUE)
    expect_true(all(subject_data$DTC <= last_obs_dtc))
  }
})


test_that("limit with individual=FALSE filters globally", {
  # Create test nif object
  test_nif <- create_test_nif_with_dtc()

  # Apply limit with individual=FALSE
  result <- limit(test_nif, individual = FALSE)

  # Verify it's still a nif object
  expect_s3_class(result, "nif")

  # Global last observation is at 16:00 (subject 1)
  # All rows should have DTC <= 16:00
  global_last_obs <- max(test_nif$DTC[test_nif$EVID == 0], na.rm = TRUE)
  expect_true(all(result$DTC <= global_last_obs))

  # Should not have any rows after 16:00
  expect_false(any(result$DTC > as.POSIXct("2024-01-01 16:00:00")))

  # Subject 2's last obs at 15:00 should be included
  expect_true(any(result$ID == 2 & result$DTC == as.POSIXct("2024-01-01 15:00:00")))
})


test_that("limit removes subjects without observations when keep_no_obs_sbs=FALSE", {
  # Create nif object with a subject that has no observations
  test_nif <- tibble::tribble(
    ~ID, ~STUDYID, ~TIME, ~DTC,                        ~EVID, ~AMT, ~CMT, ~DV, ~MDV,
    1,   "TEST01", 0,     "2024-01-01 08:00:00",      1,     100,  1,    NA,  1,  # Admin
    1,   "TEST01", 0,     "2024-01-01 08:00:00",      0,     0,    2,    5.2, 0,  # Obs
    2,   "TEST01", 0,     "2024-01-01 09:00:00",      1,     150,  1,    NA,  1,  # Admin only, no obs
    2,   "TEST01", 1,     "2024-01-01 10:00:00",      1,     150,  1,    NA,  1,  # Admin only, no obs
    3,   "TEST01", 0,     "2024-01-01 11:00:00",      1,     200,  1,    NA,  1,  # Admin
    3,   "TEST01", 0,     "2024-01-01 11:00:00",      0,     0,    2,    10.3, 0  # Obs
  ) %>%
    dplyr::mutate(DTC = as.POSIXct(.data$DTC)) %>%
    nif(silent = TRUE)

  # Apply limit with keep_no_obs_sbs=FALSE
  result <- limit(test_nif, keep_no_obs_sbs = FALSE)

  # Subject 2 should be removed (no observations)
  expect_false(any(result$ID == 2))

  # Subjects 1 and 3 should remain (have observations)
  expect_true(any(result$ID == 1))
  expect_true(any(result$ID == 3))
})


test_that("limit keeps subjects without observations when keep_no_obs_sbs=TRUE", {
  # Create nif object with a subject that has no observations
  test_nif <- tibble::tribble(
    ~ID, ~STUDYID, ~TIME, ~DTC,                        ~EVID, ~AMT, ~CMT, ~DV, ~MDV,
    1,   "TEST01", 0,     "2024-01-01 08:00:00",      1,     100,  1,    NA,  1,  # Admin
    1,   "TEST01", 0,     "2024-01-01 08:00:00",      0,     0,    2,    5.2, 0,  # Obs
    2,   "TEST01", 0,     "2024-01-01 09:00:00",      1,     150,  1,    NA,  1,  # Admin only, no obs
    2,   "TEST01", 1,     "2024-01-01 10:00:00",      1,     150,  1,    NA,  1,  # Admin only, no obs
    3,   "TEST01", 0,     "2024-01-01 11:00:00",      1,     200,  1,    NA,  1,  # Admin
    3,   "TEST01", 0,     "2024-01-01 11:00:00",      0,     0,    2,    10.3, 0  # Obs
  ) %>%
    dplyr::mutate(DTC = as.POSIXct(.data$DTC)) %>%
    nif(silent = TRUE)

  # Apply limit with keep_no_obs_sbs=TRUE
  result <- limit(test_nif, keep_no_obs_sbs = TRUE)

  # Subject 2 should be kept (even though no observations)
  expect_true(any(result$ID == 2))

  # All subjects should remain
  expect_true(any(result$ID == 1))
  expect_true(any(result$ID == 2))
  expect_true(any(result$ID == 3))
})


test_that("limit handles subjects with only observations (no administrations)", {
  # Create nif object with subject that has only observations
  test_nif <- tibble::tribble(
    ~ID, ~STUDYID, ~TIME, ~DTC,                        ~EVID, ~AMT, ~CMT, ~DV, ~MDV,
    1,   "TEST01", 0,     "2024-01-01 08:00:00",      0,     0,    2,    5.2, 0,  # Obs only
    1,   "TEST01", 4,     "2024-01-01 12:00:00",      0,     0,    2,    8.5, 0,  # Obs only
    1,   "TEST01", 8,     "2024-01-01 16:00:00",      0,     0,    2,    10.3, 0,  # Obs only (last)
    1,   "TEST01", 12,    "2024-01-01 20:00:00",      0,     0,    2,    12.1, 0   # Obs only (after last)
  ) %>%
    dplyr::mutate(DTC = as.POSIXct(.data$DTC)) %>%
    nif(silent = TRUE)

  result <- limit(test_nif, individual = TRUE)

  # Should keep all observations
  expect_equal(nrow(result), 4)
})


test_that("limit handles subjects with only administrations (no observations)", {
  # Create nif object with subject that has only administrations
  test_nif <- tibble::tribble(
    ~ID, ~STUDYID, ~TIME, ~DTC,                        ~EVID, ~AMT, ~CMT, ~DV, ~MDV,
    1,   "TEST01", 0,     "2024-01-01 08:00:00",      1,     100,  1,    NA,  1,  # Admin only
    1,   "TEST01", 4,     "2024-01-01 12:00:00",      1,     100,  1,    NA,  1,  # Admin only
    1,   "TEST01", 8,     "2024-01-01 16:00:00",      1,     100,  1,    NA,  1,  # Admin only
    2,   "TEST01", 0,     "2024-01-01 09:00:00",      1,     150,  1,    NA,  1,  # Admin
    2,   "TEST01", 0,     "2024-01-01 09:00:00",      0,     0,    2,    5.2, 0   # Obs
  ) %>%
    dplyr::mutate(DTC = as.POSIXct(.data$DTC)) %>%
    nif(silent = TRUE)

  # Apply limit with keep_no_obs_sbs=FALSE
  result <- limit(test_nif, keep_no_obs_sbs = FALSE)

  # Subject 1 should be removed (no observations)
  expect_false(any(result$ID == 1))

  # Subject 2 should remain (has observations)
  expect_true(any(result$ID == 2))
})


test_that("limit handles empty nif object", {
  # Create empty nif object
  empty_nif <- nif(silent = TRUE)

  # Should error because DTC field is missing
  expect_error(
    limit(empty_nif),
    "Missing required fields in nif object"
  )
})


test_that("limit preserves all columns", {
  # Create test nif object with extra columns
  test_nif <- tibble::tribble(
    ~ID, ~STUDYID, ~TIME, ~DTC,                        ~EVID, ~AMT, ~CMT, ~DV, ~MDV, ~ANALYTE, ~PARENT,
    1,   "TEST01", 0,     "2024-01-01 08:00:00",      1,     100,  1,    NA,  1,    "DRUG",   "DRUG",
    1,   "TEST01", 0,     "2024-01-01 08:00:00",      0,     0,    2,    5.2, 0,    "DRUG",   "DRUG",
    1,   "TEST01", 4,     "2024-01-01 12:00:00",      0,     0,    2,    8.5, 0,    "DRUG",   "DRUG",
    1,   "TEST01", 8,     "2024-01-01 16:00:00",      0,     0,    2,    10.3, 0,   "DRUG",   "DRUG",
    1,   "TEST01", 12,    "2024-01-01 20:00:00",      1,     100,  1,    NA,  1,    "DRUG",   "DRUG"
  ) %>%
    dplyr::mutate(DTC = as.POSIXct(.data$DTC)) %>%
    nif(silent = TRUE)

  # Apply limit
  result <- limit(test_nif)

  # All original columns should be preserved
  expect_true(all(names(test_nif) %in% names(result)))
  expect_true("ANALYTE" %in% names(result))
  expect_true("PARENT" %in% names(result))
})


test_that("limit handles multiple subjects with different last observation times", {
  # Create nif object with subjects having different last observation times
  test_nif <- tibble::tribble(
    ~ID, ~STUDYID, ~TIME, ~DTC,                        ~EVID, ~AMT, ~CMT, ~DV, ~MDV,
    1,   "TEST01", 0,     "2024-01-01 08:00:00",      1,     100,  1,    NA,  1,  # Admin
    1,   "TEST01", 0,     "2024-01-01 08:00:00",      0,     0,    2,    5.2, 0,  # Obs
    1,   "TEST01", 4,     "2024-01-01 12:00:00",      0,     0,    2,    8.5, 0,  # Obs (last for ID 1)
    1,   "TEST01", 8,     "2024-01-01 16:00:00",      1,     100,  1,    NA,  1,  # Admin (after last obs)
    2,   "TEST01", 0,     "2024-01-01 09:00:00",      1,     150,  1,    NA,  1,  # Admin
    2,   "TEST01", 0,     "2024-01-01 09:00:00",      0,     0,    2,    10.3, 0,  # Obs (last for ID 2)
    2,   "TEST01", 6,     "2024-01-01 15:00:00",      1,     150,  1,    NA,  1,  # Admin (after last obs)
    3,   "TEST01", 0,     "2024-01-01 10:00:00",      1,     200,  1,    NA,  1,  # Admin
    3,   "TEST01", 0,     "2024-01-01 10:00:00",      0,     0,    2,    15.2, 0,  # Obs
    3,   "TEST01", 4,     "2024-01-01 14:00:00",      0,     0,    2,    18.5, 0,  # Obs
    3,   "TEST01", 8,     "2024-01-01 18:00:00",      0,     0,    2,    20.3, 0,  # Obs (last for ID 3)
    3,   "TEST01", 12,    "2024-01-01 22:00:00",      1,     200,  1,    NA,  1   # Admin (after last obs)
  ) %>%
    dplyr::mutate(DTC = as.POSIXct(.data$DTC)) %>%
    nif(silent = TRUE)

  # Apply limit with individual=TRUE
  result <- limit(test_nif, individual = TRUE)

  # Each subject should be limited to their own last observation
  # Subject 1: last obs at 12:00
  subject1 <- result %>% dplyr::filter(.data$ID == 1)
  expect_true(all(subject1$DTC <= as.POSIXct("2024-01-01 12:00:00")))
  expect_false(any(subject1$DTC == as.POSIXct("2024-01-01 16:00:00")))

  # Subject 2: last obs at 09:00
  subject2 <- result %>% dplyr::filter(.data$ID == 2)
  expect_true(all(subject2$DTC <= as.POSIXct("2024-01-01 09:00:00")))
  expect_false(any(subject2$DTC == as.POSIXct("2024-01-01 15:00:00")))

  # Subject 3: last obs at 18:00
  subject3 <- result %>% dplyr::filter(.data$ID == 3)
  expect_true(all(subject3$DTC <= as.POSIXct("2024-01-01 18:00:00")))
  expect_false(any(subject3$DTC == as.POSIXct("2024-01-01 22:00:00")))
})


test_that("limit handles edge case with single observation per subject", {
  # Create nif object where each subject has only one observation
  test_nif <- tibble::tribble(
    ~ID, ~STUDYID, ~TIME, ~DTC,                        ~EVID, ~AMT, ~CMT, ~DV, ~MDV,
    1,   "TEST01", 0,     "2024-01-01 08:00:00",      1,     100,  1,    NA,  1,  # Admin
    1,   "TEST01", 0,     "2024-01-01 08:00:00",      0,     0,    2,    5.2, 0,  # Obs (only one)
    1,   "TEST01", 4,     "2024-01-01 12:00:00",      1,     100,  1,    NA,  1,  # Admin (after obs)
    2,   "TEST01", 0,     "2024-01-01 09:00:00",      1,     150,  1,    NA,  1,  # Admin
    2,   "TEST01", 0,     "2024-01-01 09:00:00",      0,     0,    2,    10.3, 0,  # Obs (only one)
    2,   "TEST01", 6,     "2024-01-01 15:00:00",      1,     150,  1,    NA,  1   # Admin (after obs)
  ) %>%
    dplyr::mutate(DTC = as.POSIXct(.data$DTC)) %>%
    nif(silent = TRUE)

  # Apply limit
  result <- limit(test_nif, individual = TRUE)

  # Each subject should keep only up to their single observation
  # Subject 1: should keep admin and obs at 08:00, remove admin at 12:00
  subject1 <- result %>% dplyr::filter(.data$ID == 1)
  expect_equal(nrow(subject1), 2)  # Admin and obs at 08:00
  expect_false(any(subject1$DTC == as.POSIXct("2024-01-01 12:00:00")))

  # Subject 2: should keep admin and obs at 09:00, remove admin at 15:00
  subject2 <- result %>% dplyr::filter(.data$ID == 2)
  expect_equal(nrow(subject2), 2)  # Admin and obs at 09:00
  expect_false(any(subject2$DTC == as.POSIXct("2024-01-01 15:00:00")))
})


test_that("limit handles case where last observation equals last administration", {
  # Create nif object where last observation and last administration are at same time
  test_nif <- tibble::tribble(
    ~ID, ~STUDYID, ~TIME, ~DTC,                        ~EVID, ~AMT, ~CMT, ~DV, ~MDV,
    1,   "TEST01", 0,     "2024-01-01 08:00:00",      1,     100,  1,    NA,  1,  # Admin
    1,   "TEST01", 0,     "2024-01-01 08:00:00",      0,     0,    2,    5.2, 0,  # Obs
    1,   "TEST01", 4,     "2024-01-01 12:00:00",      0,     0,    2,    8.5, 0,  # Obs
    1,   "TEST01", 8,     "2024-01-01 16:00:00",      1,     100,  1,    NA,  1,  # Admin
    1,   "TEST01", 8,     "2024-01-01 16:00:00",      0,     0,    2,    10.3, 0  # Obs (last, same as admin)
  ) %>%
    dplyr::mutate(DTC = as.POSIXct(.data$DTC)) %>%
    nif(silent = TRUE)

  # Apply limit
  result <- limit(test_nif, individual = TRUE)

  # Should keep all rows since last obs is at same time as last admin
  expect_equal(nrow(result), nrow(test_nif))
  expect_true(all(result$DTC <= as.POSIXct("2024-01-01 16:00:00")))
})


test_that("limit returns nif object", {
  # Create test nif object
  test_nif <- create_test_nif_with_dtc()

  # Apply limit
  result <- limit(test_nif)

  # Should return a nif object
  expect_s3_class(result, "nif")
  expect_true(inherits(result, "nif"))
})


test_that("limit handles NA values in DTC gracefully", {
  # Create nif object with NA DTC values
  test_nif <- tibble::tribble(
    ~ID, ~STUDYID, ~TIME, ~DTC,                        ~EVID, ~AMT, ~CMT, ~DV, ~MDV,
    1,   "TEST01", 0,     "2024-01-01 08:00:00",      1,     100,  1,    NA,  1,
    1,   "TEST01", 1,     NA_character_,               0,     0,    2,    5.2, 0,  # NA DTC
    1,   "TEST01", 4,     "2024-01-01 12:00:00",      0,     0,    2,    8.5, 0,
    1,   "TEST01", 8,     "2024-01-01 16:00:00",      0,     0,    2,    10.3, 0
  ) %>%
    dplyr::mutate(DTC = as.POSIXct(.data$DTC)) %>%
    nif(silent = TRUE)

  # Should handle NA values (max with na.rm=TRUE)
  # The function should work, but behavior with NA DTC may vary
  # This test verifies it doesn't crash
  expect_no_error(
    result <- limit(test_nif)
  )

  # Result should still be a nif object
  expect_s3_class(result, "nif")
})


test_that("limit works correctly with default parameters", {
  # Create test nif object
  test_nif <- create_test_nif_with_dtc()

  # Apply limit with default parameters (individual=TRUE, keep_no_obs_sbs=FALSE)
  result <- limit(test_nif)

  # Should behave the same as limit(test_nif, individual=TRUE, keep_no_obs_sbs=FALSE)
  result_explicit <- limit(test_nif, individual = TRUE, keep_no_obs_sbs = FALSE)

  expect_equal(result, result_explicit)
})


test_that("limit preserves data integrity", {
  # Create test nif object
  test_nif <- create_test_nif_with_dtc()

  # Apply limit
  result <- limit(test_nif, individual = TRUE)

  # Verify that all rows in result were in original
  for (i in seq_len(nrow(result))) {
    row <- result[i, ]
    # Find matching row in original (by ID, DTC, EVID)
    matching <- test_nif %>%
      dplyr::filter(
        .data$ID == row$ID,
        .data$DTC == row$DTC,
        .data$EVID == row$EVID
      )
    expect_true(nrow(matching) > 0)
  }

  # Verify that filtered rows are actually after last observation
  original_ids <- unique(test_nif$ID)
  for (id in original_ids) {
    original_subject <- test_nif %>% dplyr::filter(.data$ID == id)
    result_subject <- result %>% dplyr::filter(.data$ID == id)

    if (nrow(result_subject) > 0) {
      last_obs_dtc <- max(original_subject$DTC[original_subject$EVID == 0], na.rm = TRUE)
      removed_rows <- original_subject %>%
        dplyr::filter(!.data$DTC %in% result_subject$DTC)

      # All removed rows should be after last observation
      if (nrow(removed_rows) > 0) {
        expect_true(all(removed_rows$DTC > last_obs_dtc))
      }
    }
  }
})

