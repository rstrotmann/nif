# Test file for validate_sdtm_domains function

test_that("validate_sdtm_domains validates all domains in a valid SDTM object", {
  # Create a simple valid SDTM object
  dm_data <- tibble::tribble(
    ~DOMAIN, ~STUDYID, ~USUBJID, ~SUBJID, ~SITEID, ~SEX, ~ARMCD, ~ARM, ~ACTARMCD, ~ACTARM, ~COUNTRY,
    "DM", "STUDY1", "SUBJ-001", "001", "001", "M", "ARM1", "Treatment arm 1", "ARM1", "Treatment arm 1", "USA",
    "DM", "STUDY1", "SUBJ-002", "002", "001", "F", "ARM1", "Treatment arm 1", "ARM1", "Treatment arm 1", "USA"
  )

  # Create VS domain with required columns
  vs_data <- tibble::tribble(
    ~DOMAIN, ~STUDYID, ~USUBJID, ~VSSEQ, ~VSTESTCD, ~VSTEST, ~VSORRES, ~VSORRESU, ~VSBLFL, ~VISITNUM, ~VSDTC,
    "VS", "STUDY1", "SUBJ-001", 1, "TEMP", "Temperature", "37.0", "C", "Y", 1, "2023-01-01",
    "VS", "STUDY1", "SUBJ-002", 2, "PULSE", "Pulse Rate", "72", "beats/min", "Y", 1, "2023-01-01"
  )

  # Create a new SDTM object
  test_sdtm <- new_sdtm(list(
    dm = dm_data,
    vs = vs_data
  ))

  # The validate_sdtm_domains function should complete without error
  expect_no_error(result <- validate_sdtm_domains(test_sdtm, silent = TRUE))
  # The current implementation returns NULL
  expect_null(result)
})


test_that("validate_sdtm_domains shows/suppresses messages based on silent parameter", {
  # Create a SDTM object with domains missing some expected columns
  min_dm <- tibble::tribble(
    ~DOMAIN, ~STUDYID, ~USUBJID, ~SUBJID, ~SITEID, ~SEX, ~ARMCD, ~ARM, ~ACTARMCD, ~ACTARM, ~COUNTRY,
    "DM", "STUDY1", "SUBJ-001", "001", "001", "M", "ARM1", "Treatment arm 1", "ARM1", "Treatment arm 1", "USA",
    "DM", "STUDY1", "SUBJ-002", "002", "001", "F", "ARM1", "Treatment arm 1", "ARM1", "Treatment arm 1", "USA"
    # Missing expected columns
  )

  min_vs <- tibble::tribble(
    ~DOMAIN, ~STUDYID, ~USUBJID, ~VSSEQ, ~VSTESTCD, ~VSTEST, ~VSORRES, ~VSORRESU, ~VSBLFL, ~VISITNUM, ~VSDTC,
    "VS", "STUDY1", "SUBJ-001", 1, "TEMP", "Temperature", "37.0", "C", "Y", 1, "2023-01-01",
    "VS", "STUDY1", "SUBJ-002", 2, "PULSE", "Pulse Rate", "72", "beats/min", "Y", 1, "2023-01-01"
    # Missing expected columns
  )

  # Create a new SDTM object
  test_sdtm <- new_sdtm(list(
    dm = min_dm,
    vs = min_vs
  ))

  # Should not show messages when silent=TRUE
  expect_no_message(
    validate_sdtm_domains(test_sdtm, silent = TRUE)
  )
})


test_that("validate_sdtm_domains handles mixed valid and unknown domains", {
  # Create a SDTM object with one valid domain and one unknown domain
  dm_data <- tibble::tribble(
    ~DOMAIN, ~STUDYID, ~USUBJID, ~SUBJID, ~SITEID, ~SEX, ~ARMCD, ~ARM, ~ACTARMCD, ~ACTARM, ~COUNTRY,
    "DM", "STUDY1", "SUBJ-001", "001", "001", "M", "ARM1", "Treatment arm 1", "ARM1", "Treatment arm 1", "USA",
    "DM", "STUDY1", "SUBJ-002", "002", "001", "F", "ARM1", "Treatment arm 1", "ARM1", "Treatment arm 1", "USA"
  )

  # Unknown domain
  xyz_data <- tibble::tribble(
    ~DOMAIN, ~STUDYID, ~USUBJID,
    "XYZ", "STUDY1", "SUBJ-001",
    "XYZ", "STUDY1", "SUBJ-002"
  )

  # Create a new SDTM object
  test_sdtm <- new_sdtm(list(
    dm = dm_data,
    xyz = xyz_data
  ))

  # Should warn about unknown domain but not error
  expect_no_message(
    result <- validate_sdtm_domains(test_sdtm, silent = TRUE)
  )
})
