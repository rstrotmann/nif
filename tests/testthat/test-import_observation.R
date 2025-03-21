# Helper function to create a minimal nif object for testing
create_test_nif <- function() {
  data.frame(
    USUBJID = c("SUBJ-001", "SUBJ-002"),
    ID = c(1, 2),
    ANALYTE = c("DRUG", "DRUG"),
    CMT = c(1, 1),
    AMT = c(100, 150),
    DOSE = c(100, 150),
    DTC = ymd_hms(c("2023-01-01 08:00:00", "2023-01-01 08:00:00")),
    NTIME = c(0, 0),
    TIME = c(0, 0),
    PARENT = c("DRUG", "DRUG"),
    METABOLITE = c(FALSE, FALSE),
    EVID = c(1, 1),
    MDV = c(1, 1),
    DV = c(NA, NA),
    IMPUTATION = c("", ""),
    stringsAsFactors = FALSE
  ) %>%
    structure(class = c("nif", "tbl_df", "tbl", "data.frame"))
}

# Helper function to create test raw observation data
create_test_raw <- function(with_dtc = TRUE, with_ntime = TRUE) {
  df <- tibble::tribble(
      ~USUBJID, ~DV_VALUE,
    "SUBJ-001",      10.5,
    "SUBJ-001",       5.2,
    "SUBJ-002",       8.7
  )

  if (with_dtc) {
    df$OBS_DTC <- ymd_hms(c("2023-01-01 10:00:00", "2023-01-01 14:00:00", "2023-01-01 10:00:00"))
  }

  if (with_ntime) {
    df$OBS_NTIME <- c(2, 6, 2)
  }

  return(df)
}


test_that("import_observation validates input parameters correctly", {
  # Test non-nif object
  expect_error(
    import_observation(
      data.frame(),
      create_test_raw(),
      analyte = "TEST",
      DV_field = "DV_VALUE"
    ),
    "nif must be an nif object"
  )

  test_nif <- create_test_nif()

  # Test missing required fields
  expect_error(
    import_observation(
      test_nif,
      create_test_raw(),
      analyte = "TEST"
    )
  )

  # Test missing time fields
  expect_error(
    import_observation(
      test_nif,
      create_test_raw(with_dtc = FALSE, with_ntime = FALSE),
      analyte = "TEST",
      DV_field = "DV_VALUE"
    ),
    "ERROR: One of the time fields .* must be present"
  )

  # Test empty filtered data
  expect_error(
    import_observation(
      test_nif,
      create_test_raw(),
      analyte = "TEST",
      parent = "DRUG",
      cmt = 2,
      DV_field = "DV_VALUE",
      DTC_field = "OBS_DTC",
      observation_filter = "USUBJID == 'NON-EXISTENT'"
    ),
    "The observation_filter .* returned no entries"
  )


  # Test missing parent and no administrations
  # empty_nif <- structure(
  #   data.frame(),
  #   class = c("nif", "tbl_df", "tbl", "data.frame")
  # )
  empty_nif <- new_nif()

  expect_error(
    import_observation(
      empty_nif,
      create_test_raw(),
      analyte = "TEST",
      DV_field = "DV_VALUE"
    ),
    "Please add at least one administration first!"
  )
})


test_that("import_observation correctly handles DTC field", {
  test_nif <- create_test_nif()
  test_raw <- create_test_raw(with_dtc = TRUE, with_ntime = FALSE)

  result <- import_observation(
    test_nif,
    test_raw,
    analyte = "TEST",
    parent = "DRUG",
    cmt = 2,
    DV_field = "DV_VALUE",
    DTC_field = "OBS_DTC",
    NTIME_field = NA
  )

  # Check structure
  expect_s3_class(result, "nif")

  # Original records plus new observation records
  expect_equal(nrow(result), nrow(test_nif) + nrow(test_raw))

  # Check observation records
  obs_records <- result %>% filter(EVID == 0)
  expect_equal(nrow(obs_records), nrow(test_raw))
  expect_equal(obs_records$ANALYTE, rep("TEST", nrow(test_raw)))
  expect_equal(obs_records$PARENT, rep("DRUG", nrow(test_raw)))
  expect_equal(obs_records$CMT, rep(2, nrow(test_raw)))

  # Check DTC was preserved
  expect_equal(obs_records$DTC, test_raw$OBS_DTC)
})


test_that("import_observation correctly handles NTIME field", {
  test_nif <- create_test_nif()
  test_raw <- create_test_raw(with_dtc = FALSE, with_ntime = TRUE)

  result <- import_observation(
    test_nif,
    test_raw,
    analyte = "TEST",
    parent = "DRUG",
    cmt = 2,
    DV_field = "DV_VALUE",
    DTC_field = NULL,
    NTIME_field = "OBS_NTIME"
  )

  # Check observation records
  obs_records <- result %>% filter(EVID == 0)
  expect_equal(nrow(obs_records), nrow(test_raw))

  # Check NTIME was preserved
  expect_equal(obs_records$NTIME, test_raw$OBS_NTIME)

  # Check DTC was derived from NTIME
  expect_false(any(is.na(obs_records$DTC)))
  expect_true(all(obs_records$IMPUTATION == "DTC derived from OBS_NTIME"))

  # Check each subject's observation time is relative to their administration time
  for (id in unique(test_raw$USUBJID)) {
    admin_time <- test_nif %>%
      filter(USUBJID == id, EVID == 1) %>%
      pull(DTC)

    obs_time <- obs_records %>%
      filter(USUBJID == id) %>%
      pull(DTC)

    raw_ntime <- test_raw %>%
      filter(USUBJID == id) %>%
      pull(OBS_NTIME)

    for (i in seq_along(obs_time)) {
      expected_time <- admin_time + duration(hours = raw_ntime[i])
      expect_equal(obs_time[i], expected_time)
    }
  }
})


# test_that("import_observation automatically assigns compartment when cmt is NULL", {
#   test_nif <- create_test_nif()
#   test_raw <- create_test_raw()
#
#   # Capture messages to verify warnings
#   expect_message(
#     result <- import_observation(
#       test_nif,
#       test_raw,
#       analyte = "TEST",
#       parent = "DRUG",
#       cmt = NULL,
#       DV_field = "DV_VALUE",
#       DTC_field = "OBS_DTC"
#     ),
#     "Compartment was not specified and has been set to"
#   )
#
#   # Check if compartment was automatically assigned
#   obs_records <- result %>% filter(EVID == 0)
#   max_cmt_in_nif <- max(test_nif$CMT)
#   expect_equal(unique(obs_records$CMT), max_cmt_in_nif + 1)
# })
#
#
# test_that("import_observation automatically determines parent when parent is NULL", {
#   test_nif <- create_test_nif()
#   test_raw <- create_test_raw()
#
#   # Case 1: analyte matches an existing ANALYTE in nif
#   expect_message(
#     result1 <- import_observation(
#       test_nif,
#       test_raw,
#       analyte = "DRUG", # Matches existing analyte
#       parent = NULL,
#       cmt = 2,
#       DV_field = "DV_VALUE",
#       DTC_field = "OBS_DTC"
#     ),
#     NA
#   )
#
#   obs_records1 <- result1 %>% filter(EVID == 0)
#   expect_equal(unique(obs_records1$PARENT), "DRUG")
#
#   # Case 2: analyte doesn't match existing ANALYTE
#   expect_message(
#     result2 <- import_observation(
#       test_nif,
#       test_raw,
#       analyte = "METABOLITE", # Doesn't match existing analyte
#       parent = NULL,
#       cmt = 3,
#       DV_field = "DV_VALUE",
#       DTC_field = "OBS_DTC"
#     ),
#     "Parent for .* was set to"
#   )
#
#   obs_records2 <- result2 %>% filter(EVID == 0)
#   expect_equal(unique(obs_records2$PARENT), "DRUG") # Should be guessed from nif
# })
#
#
# test_that("import_observation correctly joins subject data", {
#   # Create test nif with additional subject data
#   test_nif <- create_test_nif() %>%
#     mutate(
#       BL_WT = c(70, 85),
#       BL_HT = c(175, 180)
#     )
#
#   test_raw <- create_test_raw()
#
#   result <- import_observation(
#     test_nif,
#     test_raw,
#     analyte = "TEST",
#     parent = "DRUG",
#     cmt = 2,
#     DV_field = "DV_VALUE",
#     DTC_field = "OBS_DTC",
#     keep = c("BL_WT", "BL_HT")
#   )
#
#   # Check if subject data was correctly joined
#   obs_records <- result %>% filter(EVID == 0)
#
#   # For each subject, check if baseline values match
#   for (id in unique(test_raw$USUBJID)) {
#     obs_subject <- obs_records %>% filter(USUBJID == id)
#     nif_subject <- test_nif %>% filter(USUBJID == id)
#
#     expect_equal(unique(obs_subject$BL_WT), unique(nif_subject$BL_WT))
#     expect_equal(unique(obs_subject$BL_HT), unique(nif_subject$BL_HT))
#   }
# })
#
#
# test_that("import_observation correctly sets debug fields", {
#   test_nif <- create_test_nif()
#   test_raw <- create_test_raw()
#
#   # Without debug
#   result1 <- import_observation(
#     test_nif,
#     test_raw,
#     analyte = "TEST",
#     parent = "DRUG",
#     cmt = 2,
#     DV_field = "DV_VALUE",
#     DTC_field = "OBS_DTC",
#     debug = FALSE
#   )
#
#   # With debug
#   result2 <- import_observation(
#     test_nif,
#     test_raw,
#     analyte = "TEST",
#     parent = "DRUG",
#     cmt = 2,
#     DV_field = "DV_VALUE",
#     DTC_field = "OBS_DTC",
#     debug = TRUE
#   )
#
#   # Check if debug fields are included when debug = TRUE
#   expect_true(all(c("SRC_DOMAIN", "SRC_SEQ") %in% names(result2)))
#
#   # For observations, check if SRC_DOMAIN is set to "IMPORT"
#   obs_records <- result2 %>% filter(EVID == 0)
#   expect_equal(unique(obs_records$SRC_DOMAIN), "IMPORT")
#   expect_true(all(is.na(obs_records$SRC_SEQ)))
# })
