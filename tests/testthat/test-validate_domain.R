# Test file for validate_domain function

test_that("validate_domain accepts valid domain", {
  # Create a valid domain with all required columns for DM
  dm_data <- tibble::tribble(
    ~DOMAIN, ~STUDYID,   ~USUBJID, ~SUBJID, ~SITEID, ~SEX, ~ARMCD,              ~ARM, ~ACTARMCD,           ~ACTARM, ~COUNTRY,
       "DM", "STUDY1", "SUBJ-001",   "001",   "001",  "M", "ARM1", "Treatment arm 1",    "ARM1", "Treatment arm 1",    "USA"
  )

  # Should run without error
  expect_true(validate_domain(dm_data, silent = TRUE))
})


test_that("validate_domain rejects non-data frame input", {
  # Test with a non-data frame input
  expect_error(
    validate_domain("not a data frame"),
    "The 'domain' parameter must be a data frame"
  )

  # Test with NULL
  expect_error(
    validate_domain(NULL),
    "The 'domain' parameter must be a data frame"
  )
})


test_that("validate_domain requires DOMAIN column", {
  # Create a data frame without DOMAIN column
  invalid_domain <- tibble::tribble(
    ~STUDYID,   ~USUBJID,
    "STUDY1", "SUBJ-001"
  )

  expect_error(
    validate_domain(invalid_domain),
    "The data frame must have a DOMAIN column"
  )
})


test_that("validate_domain rejects empty DOMAIN column", {
  # Create a data frame with empty DOMAIN column
  invalid_domain <- tibble::tribble(
    ~DOMAIN, ~STUDYID, ~USUBJID
  )

  expect_error(
    validate_domain(invalid_domain),
    "The DOMAIN column is empty"
  )
})


test_that("validate_domain handles multiple DOMAIN values", {
  # Create a data frame with multiple DOMAIN values and all required columns for DM
  mixed_domain <- tibble::tribble(
    ~DOMAIN, ~STUDYID,   ~USUBJID, ~SUBJID, ~SITEID, ~SEX, ~ARMCD,              ~ARM, ~ACTARMCD,           ~ACTARM, ~COUNTRY,
       "DM", "STUDY1", "SUBJ-001",   "001",   "001",  "M", "ARM1", "Treatment arm 1",    "ARM1", "Treatment arm 1",    "USA",
       "DM", "STUDY1", "SUBJ-002",   "002",   "001",  "F", "ARM1", "Treatment arm 1",    "ARM1", "Treatment arm 1",    "USA"
  )

  # Should not warn about multiple domains since they're the same
  expect_true(validate_domain(mixed_domain, silent = TRUE))

  # Now create a domain with truly mixed domain values
  mixed_domain$DOMAIN[2] <- "VS"

  # Should warn about multiple domains, but continue
  expect_error(
    validate_domain(mixed_domain, silent = TRUE),
    "Multiple domain values found: DM and VS"
  )
})


test_that("validate_domain warns about missing expected columns", {
  # Create a domain with only required columns for DM
  minimal_dm <- tibble::tribble(
    ~DOMAIN, ~STUDYID,   ~USUBJID, ~SUBJID, ~SITEID, ~SEX, ~ARMCD,              ~ARM, ~ACTARMCD,           ~ACTARM, ~COUNTRY,
       "DM", "STUDY1", "SUBJ-001",   "001",   "001",  "M", "ARM1", "Treatment arm 1",    "ARM1", "Treatment arm 1",    "USA"
    # Missing expected columns like RFSTDTC, AGE
  )

  # Should warn about missing expected columns when silent=FALSE
  expect_message(
    expect_message(
      validate_domain(minimal_dm, silent = FALSE),
      "The following expected columns are missing in domain DM"
    ),
    "The following permitted columns are missing in domain DM"
  )

  # Should not show message if silent=TRUE
  expect_no_message(
    validate_domain(minimal_dm, silent = TRUE)
  )
})


test_that("validate_domain warns about missing permitted columns", {
  # Create a domain with required columns for DM
  basic_dm <- tibble::tribble(
    ~DOMAIN, ~STUDYID,   ~USUBJID, ~SUBJID, ~SITEID, ~SEX, ~ARMCD,              ~ARM, ~ACTARMCD,           ~ACTARM, ~COUNTRY, ~AGE,
       "DM", "STUDY1", "SUBJ-001",   "001",   "001",  "M", "ARM1", "Treatment arm 1",    "ARM1", "Treatment arm 1",    "USA",   35
    # Missing permitted columns like RACE, ETHNIC, etc.
  )

  # Should warn about missing permitted columns when silent=FALSE
    expect_message(
      expect_message(
        validate_domain(basic_dm, silent = FALSE),
        "The following permitted columns are missing in domain DM"
      ),
      "The following expected columns are missing in domain DM"
    )

  # Should not show message if silent=TRUE
  expect_no_message(
    validate_domain(basic_dm, silent = TRUE)
  )
})


test_that("validate_domain handles unknown domains gracefully", {
  # Create a data frame with an unknown domain
  unknown_domain <- tibble::tribble(
    ~DOMAIN, ~STUDYID,   ~USUBJID,
      "XYZ", "STUDY1", "SUBJ-001"
  )

  # Should warn about unknown domain but not error
  expect_message(
    validate_domain(unknown_domain, silent = FALSE),
    "Unknown domain 'XYZ' cannot be validated!"
  )
})


test_that("validate_domain handles correctly case sensitivity", {
  # Create a domain with lowercase domain name but with all required fields
  lowercase_domain <- tibble::tribble(
    ~DOMAIN, ~STUDYID,   ~USUBJID, ~SUBJID, ~SITEID, ~SEX, ~ARMCD,              ~ARM, ~ACTARMCD,           ~ACTARM, ~COUNTRY,
       "dm", "STUDY1", "SUBJ-001",   "001",   "001",  "M", "ARM1", "Treatment arm 1",    "ARM1", "Treatment arm 1",    "USA"
  )

  # Should run without error (case insensitive)
  expect_true(validate_domain(lowercase_domain, silent = TRUE))
})


test_that("validate_domain works with example data", {
  # Test with package example data if available
  skip_if_not_installed("nif")
  skip_if_not(exists("examplinib_fe"), "Example SDTM data not available")

  if (exists("examplinib_fe")) {
    # Get DM domain from example data
    dm_domain <- domain(examplinib_fe, "dm")

    # Should validate without error
    expect_true(validate_domain(dm_domain, silent = TRUE))
  }
})

