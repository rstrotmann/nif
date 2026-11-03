# Test file for validate_domain function

test_that("validate_domain accepts valid domain", {
  # Create a valid domain with all required columns for DM
  dm_data <- tibble::tribble(
    ~DOMAIN, ~STUDYID, ~USUBJID, ~SUBJID, ~SITEID, ~SEX, ~ARMCD, ~ARM, ~ACTARMCD, ~ACTARM, ~COUNTRY,
    "DM", "STUDY1", "SUBJ-001", "001", "001", "M", "ARM1", "Treatment arm 1", "ARM1", "Treatment arm 1", "USA"
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
    ~DOMAIN, ~STUDYID, ~USUBJID, ~SUBJID, ~SITEID, ~SEX, ~ARMCD, ~ARM, ~ACTARMCD, ~ACTARM, ~COUNTRY,
    "DM", "STUDY1", "SUBJ-001", "001", "001", "M", "ARM1", "Treatment arm 1", "ARM1", "Treatment arm 1", "USA",
    "DM", "STUDY1", "SUBJ-002", "002", "001", "F", "ARM1", "Treatment arm 1", "ARM1", "Treatment arm 1", "USA"
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
    ~DOMAIN, ~STUDYID, ~USUBJID, ~SUBJID, ~SITEID, ~SEX, ~ARMCD, ~ARM, ~ACTARMCD, ~ACTARM, ~COUNTRY,
    "DM", "STUDY1", "SUBJ-001", "001", "001", "M", "ARM1", "Treatment arm 1", "ARM1", "Treatment arm 1", "USA"
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
    ~DOMAIN, ~STUDYID, ~USUBJID, ~SUBJID, ~SITEID, ~SEX, ~ARMCD, ~ARM, ~ACTARMCD, ~ACTARM, ~COUNTRY, ~AGE,
    "DM", "STUDY1", "SUBJ-001", "001", "001", "M", "ARM1", "Treatment arm 1", "ARM1", "Treatment arm 1", "USA", 35
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
    ~DOMAIN, ~STUDYID, ~USUBJID,
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
    ~DOMAIN, ~STUDYID, ~USUBJID, ~SUBJID, ~SITEID, ~SEX, ~ARMCD, ~ARM, ~ACTARMCD, ~ACTARM, ~COUNTRY,
    "dm", "STUDY1", "SUBJ-001", "001", "001", "M", "ARM1", "Treatment arm 1", "ARM1", "Treatment arm 1", "USA"
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


test_that("validate_domain signals sdtm_arg_error for non-data frame input", {
  expect_error(validate_domain("not a data frame"), class = "sdtm_arg_error")
  expect_error(validate_domain(NULL), class = "sdtm_arg_error")
  expect_error(validate_domain(list(DOMAIN = "DM")), class = "sdtm_arg_error")
})


test_that("validate_domain signals sdtm_arg_error for missing DOMAIN column", {
  no_domain <- tibble::tribble(
    ~STUDYID, ~USUBJID,
    "STUDY1", "SUBJ-001"
  )

  expect_error(validate_domain(no_domain), class = "sdtm_arg_error")
})


test_that("validate_domain signals sdtm_arg_error for empty DOMAIN column", {
  # zero rows
  no_rows <- tibble::tribble(
    ~DOMAIN, ~STUDYID, ~USUBJID
  )
  expect_error(validate_domain(no_rows), class = "sdtm_arg_error")

  # only empty strings
  blank <- tibble::tribble(
    ~DOMAIN, ~STUDYID, ~USUBJID,
    "",      "STUDY1", "SUBJ-001",
    "",      "STUDY1", "SUBJ-002"
  )
  expect_error(
    validate_domain(blank),
    "The DOMAIN column is empty",
    class = "sdtm_arg_error"
  )
})


test_that("validate_domain signals sdtm_arg_error for multiple domains", {
  mixed <- tibble::tribble(
    ~DOMAIN, ~USUBJID,
    "DM",    "SUBJ-001",
    "VS",    "SUBJ-002",
    "LB",    "SUBJ-003"
  )

  expect_error(
    validate_domain(mixed, silent = TRUE),
    "Multiple domain values found: DM, VS and LB",
    class = "sdtm_arg_error"
  )
})


test_that("validate_domain reports error in the calling function", {
  check_input <- function(x) validate_domain(x)

  err <- expect_error(check_input(1), class = "sdtm_arg_error")
  expect_equal(rlang::call_name(err$call), "check_input")

  explicit_call <- function(x) validate_domain(x, call = rlang::current_env())
  err <- expect_error(explicit_call(1), class = "sdtm_arg_error")
  expect_equal(rlang::call_name(err$call), "explicit_call")
})


test_that("validate_domain does not error on unknown domain and returns TRUE", {
  unknown <- tibble::tribble(
    ~DOMAIN, ~STUDYID, ~USUBJID,
    "XYZ",   "STUDY1", "SUBJ-001"
  )

  expect_no_error(validate_domain(unknown, silent = TRUE))
  expect_invisible(validate_domain(unknown, silent = TRUE))
  expect_true(validate_domain(unknown, silent = TRUE))
  expect_no_message(validate_domain(unknown, silent = TRUE))
})


test_that("validate_domain reports missing required columns", {
  # COUNTRY and ACTARM are required for DM
  incomplete_dm <- tibble::tribble(
    ~DOMAIN, ~STUDYID, ~USUBJID,   ~SUBJID, ~SITEID, ~SEX, ~ARMCD, ~ARM,              ~ACTARMCD,
    "DM",    "STUDY1", "SUBJ-001", "001",   "001",   "M",  "ARM1", "Treatment arm 1", "ARM1"
  )

  msgs <- testthat::capture_messages(
    validate_domain(incomplete_dm, silent = FALSE)
  )
  expect_true(
    any(grepl(
      "The following required columns are missing in DM: ACTARM, COUNTRY",
      msgs, fixed = TRUE
    ))
  )

  # required columns are reported, but do not cause an error
  expect_true(validate_domain(incomplete_dm, silent = TRUE))
  expect_no_message(validate_domain(incomplete_dm, silent = TRUE))
})


test_that("validate_domain reports no required-column message for valid DM", {
  dm <- tibble::tribble(
    ~DOMAIN, ~STUDYID, ~USUBJID,   ~SUBJID, ~SITEID, ~SEX, ~ARMCD, ~ARM,              ~ACTARMCD, ~ACTARM,           ~COUNTRY,
    "DM",    "STUDY1", "SUBJ-001", "001",   "001",   "M",  "ARM1", "Treatment arm 1", "ARM1",     "Treatment arm 1", "USA"
  )

  msgs <- testthat::capture_messages(validate_domain(dm, silent = FALSE))
  expect_false(any(grepl("required columns", msgs)))
})


test_that("validate_domain honours the global silent option", {
  unknown <- tibble::tribble(
    ~DOMAIN, ~STUDYID, ~USUBJID,
    "XYZ",   "STUDY1", "SUBJ-001"
  )

  # explicit silent argument wins
  expect_message(
    validate_domain(unknown, silent = FALSE),
    "Unknown domain 'XYZ' cannot be validated!"
  )
  expect_no_message(validate_domain(unknown, silent = TRUE))
})
