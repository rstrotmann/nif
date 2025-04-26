test_that("impute_exendtc_to_cutoff works correctly", {
  # Create test data using tribbles
  ex_input <- tibble::tribble(
    ~USUBJID, ~EXTRT,      ~EXSEQ, ~EXSTDTC,            ~EXENDTC,            ~IMPUTATION,
    "001",    "Drug A",    1,      "2023-01-01 08:00",  "2023-01-14 08:00",  "",
    "001",    "Drug A",    2,      "2023-01-15 08:00",  NA,                  "",
    "002",    "Drug A",    1,      "2023-01-01 09:00",  "2023-01-14 09:00",  "",
    "002",    "Drug A",    2,      "2023-01-15 09:00",  "2023-01-30 09:00",  "",
    "003",    "Drug B",    1,      "2023-01-01 10:00",  "2023-01-14 10:00",  "",
    "003",    "Drug B",    2,      "2023-01-15 10:00",  NA,                  "",
    "004",    "Drug A",    1,      "2023-01-01 11:00",  "2023-01-14 11:00",  "existing imputation",
    "004",    "Drug A",    2,      "2023-01-15 11:00",  NA_character_,       "existing imputation"
  )

  # Define cut-off date
  cut_off_date <- as.POSIXct("2023-02-15 23:59", tz = "UTC")

  # Expected output
  expected_output <- tibble::tribble(
    ~USUBJID, ~EXTRT,      ~EXSEQ, ~EXSTDTC,            ~EXENDTC,            ~IMPUTATION,
    "001",    "Drug A",    1,      "2023-01-01 08:00:00",  "2023-01-14 08:00:00",  "",
    "001",    "Drug A",    2,      "2023-01-15 08:00:00",  "2023-02-15 23:59:00",  "missing EXENDTC set to data cutoff",
    "002",    "Drug A",    1,      "2023-01-01 09:00:00",  "2023-01-14 09:00:00",  "",
    "002",    "Drug A",    2,      "2023-01-15 09:00:00",  "2023-01-30 09:00:00",  "",
    "003",    "Drug B",    1,      "2023-01-01 10:00:00",  "2023-01-14 10:00:00",  "",
    "003",    "Drug B",    2,      "2023-01-15 10:00:00",  "2023-02-15 23:59:00",  "missing EXENDTC set to data cutoff",
    "004",    "Drug A",    1,      "2023-01-01 11:00:00",  "2023-01-14 11:00:00",  "existing imputation",
    "004",    "Drug A",    2,      "2023-01-15 11:00:00",  "2023-02-15 23:59:00",  "missing EXENDTC set to data cutoff"
  )

  # Run function with silent = TRUE to suppress messages during testing
  result <- impute_exendtc_to_cutoff(ex_input, cut_off_date, silent = TRUE)

  # Make sure dates have the same class for comparison
  result <- result %>%
    mutate(across(c(EXSTDTC, EXENDTC), as.character))

  expected_output <- expected_output %>%
    mutate(across(c(EXSTDTC, EXENDTC), as.character))

  # Test that the function correctly imputes missing EXENDTC values
  expect_equal(result, expected_output)

  # Test with no missing EXENDTC in last rows
  ex_complete <- tibble::tribble(
    ~USUBJID, ~EXTRT,      ~EXSEQ, ~EXSTDTC,            ~EXENDTC,            ~IMPUTATION,
    "001",    "Drug A",    1,      "2023-01-01 08:00:00",  "2023-01-14 08:00:00",  "",
    "001",    "Drug A",    2,      "2023-01-15 08:00:00",  "2023-01-30 08:00:00",  ""
  )

  # The result should be the same as input when no imputation is needed
  result_complete <- impute_exendtc_to_cutoff(ex_complete, cut_off_date, silent = TRUE)

  # Convert to character for comparison
  result_complete <- result_complete %>%
    mutate(across(c(EXSTDTC, EXENDTC), as.character))

  ex_complete <- ex_complete %>%
    mutate(across(c(EXSTDTC, EXENDTC), as.character))

  expect_equal(result_complete, ex_complete)

  # Test with multiple treatments per subject
  ex_multi_treatment <- tibble::tribble(
    ~USUBJID, ~EXTRT,      ~EXSEQ, ~EXSTDTC,            ~EXENDTC,            ~IMPUTATION,
    "001",    "Drug A",    1,      "2023-01-01 08:00:00",  "2023-01-14 08:00:00",  "",
    "001",    "Drug A",    2,      "2023-01-15 08:00:00",  NA_character_,       "",
    "001",    "Drug B",    1,      "2023-01-01 09:00:00",  "2023-01-14 09:00:00",  "",
    "001",    "Drug B",    2,      "2023-01-15 09:00:00",  NA_character_,       ""
  )

  expected_multi_treatment <- tibble::tribble(
    ~USUBJID, ~EXTRT,      ~EXSEQ, ~EXSTDTC,            ~EXENDTC,            ~IMPUTATION,
    "001",    "Drug A",    1,      "2023-01-01 08:00:00",  "2023-01-14 08:00:00",  "",
    "001",    "Drug A",    2,      "2023-01-15 08:00:00",  "2023-02-15 23:59:00",  "missing EXENDTC set to data cutoff",
    "001",    "Drug B",    1,      "2023-01-01 09:00:00",  "2023-01-14 09:00:00",  "",
    "001",    "Drug B",    2,      "2023-01-15 09:00:00",  "2023-02-15 23:59:00",  "missing EXENDTC set to data cutoff"
  )

  result_multi <- impute_exendtc_to_cutoff(ex_multi_treatment, cut_off_date, silent = TRUE)

  # Convert to character for comparison
  result_multi <- result_multi %>%
    mutate(across(c(EXSTDTC, EXENDTC), as.character))

  expected_multi_treatment <- expected_multi_treatment %>%
    mutate(across(c(EXSTDTC, EXENDTC), as.character))

  expect_equal(result_multi, expected_multi_treatment)

  # Test with IMPUTATION column missing
  ex_no_imputation_col <- tibble::tribble(
    ~USUBJID, ~EXTRT,      ~EXSEQ, ~EXSTDTC,            ~EXENDTC,
    "001",    "Drug A",    1,      "2023-01-01 08:00:00",  "2023-01-14 08:00:00",
    "001",    "Drug A",    2,      "2023-01-15 08:00:00",  NA_character_
  )

  # This should cause an error since impute_exendtc_to_cutoff assumes IMPUTATION column
  expect_error(impute_exendtc_to_cutoff(ex_no_imputation_col, cut_off_date, silent = TRUE))
})
