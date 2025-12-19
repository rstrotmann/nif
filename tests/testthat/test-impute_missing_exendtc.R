test_that("impute_missing_exendtc validates input correctly", {
  # Test missing required columns
  ex_missing_cols <- tibble::tribble(
    ~USUBJID, ~EXTRT,
    "SUBJ-001", "DRUG A"
  )

  expect_error(
    impute_missing_exendtc(ex_missing_cols),
    "Missing colums in domain EX: EXSTDTC and EXENDTC"
  )
})


test_that("impute_missing_exendtc handles basic imputation correctly", {
  # Create test data with missing EXENDTC in middle row
  ex <- tibble::tribble(
    ~USUBJID, ~EXTRT, ~EXSEQ, ~EXSTDTC, ~EXENDTC, ~IMPUTATION,
    "SUBJ-001", "DRUG A", 1, "2023-01-01T08:00", "2023-01-14T08:00", "",
    "SUBJ-001", "DRUG A", 2, "2023-01-15T08:00", NA, "",
    "SUBJ-001", "DRUG A", 3, "2023-01-29T08:00", "2023-02-10T08:00", ""
  ) %>% lubrify_dates()

  # Capture output to avoid message cluttering test results
  result <- capture_messages(
    out <- impute_missing_exendtc(ex)
  )

  # Check that middle row was imputed to the day before the next administration
  middle_row <- out[out$USUBJID == "SUBJ-001" & out$EXSEQ == 2, ]

  expect_equal(as.character(middle_row$EXENDTC), "2023-01-28 08:00:00")
  expect_equal(middle_row$IMPUTATION, "EXENDTC imputed as the day before the next EXSTDTC")
})


test_that("impute_missing_exendtc does not modify data when no imputations needed", {
  # Create test data with no missing EXENDTC values
  ex <- tibble::tribble(
    ~USUBJID, ~EXTRT, ~EXSEQ, ~EXSTDTC, ~EXENDTC, ~IMPUTATION,
    "SUBJ-001", "DRUG A", 1, "2023-01-01T08:00", "2023-01-14T08:00", "",
    "SUBJ-001", "DRUG A", 2, "2023-01-15T08:00", "2023-01-28T08:00", "",
    "SUBJ-001", "DRUG A", 3, "2023-01-29T08:00", "2023-02-10T08:00", ""
  ) %>% lubrify_dates()

  result <- impute_missing_exendtc(ex)

  # Check that original data is unchanged
  expect_equal(result, ex)
})


test_that("impute_missing_exendtc creates IMPUTATION column if missing", {
  # Create test data without IMPUTATION column
  ex <- tibble::tribble(
    ~USUBJID, ~EXTRT, ~EXSEQ, ~EXSTDTC, ~EXENDTC,
    "SUBJ-001", "DRUG A", 1, "2023-01-01T08:00", "2023-01-14T08:00",
    "SUBJ-001", "DRUG A", 2, "2023-01-15T08:00", NA,
    "SUBJ-001", "DRUG A", 3, "2023-01-29T08:00", "2023-02-10T08:00"
  ) %>% lubrify_dates()

  # Capture output to avoid message cluttering test results
  result <- capture_messages(
    out <- impute_missing_exendtc(ex)
  )

  # Check that IMPUTATION column was added
  expect_true("IMPUTATION" %in% names(out))

  # Check that middle row has imputation message
  middle_row <- out[out$USUBJID == "SUBJ-001" & out$EXSEQ == 2, ]
  expect_equal(middle_row$IMPUTATION, "EXENDTC imputed as the day before the next EXSTDTC")
})


test_that("impute_missing_exendtc does not impute last administrations", {
  # Create test data with missing EXENDTC in last row
  ex <- tibble::tribble(
    ~USUBJID, ~EXTRT, ~EXSEQ, ~EXSTDTC, ~EXENDTC, ~IMPUTATION,
    "SUBJ-001", "DRUG A", 1, "2023-01-01T08:00", "2023-01-14T08:00", "",
    "SUBJ-001", "DRUG A", 2, "2023-01-15T08:00", NA, "",
    "SUBJ-001", "DRUG A", 3, "2023-01-29T08:00", NA, ""
  ) %>% lubrify_dates()

  # Capture output to avoid message cluttering test results
  result <- capture_messages(
    out <- impute_missing_exendtc(ex)
  )

  # Check that middle row was imputed
  middle_row <- out[out$USUBJID == "SUBJ-001" & out$EXSEQ == 2, ]
  expect_equal(as.character(middle_row$EXENDTC), "2023-01-28 08:00:00")

  # Check that last row was NOT imputed (it should be handled by a different function)
  last_row <- out[out$USUBJID == "SUBJ-001" & out$EXSEQ == 3, ]
  expect_true(is.na(last_row$EXENDTC))
  expect_equal(last_row$IMPUTATION, "")
})


test_that("impute_missing_exendtc handles multiple subjects and treatments", {
  # Create test data with multiple subjects and treatments
  ex <- tibble::tribble(
    ~USUBJID, ~EXTRT, ~EXSEQ, ~EXSTDTC, ~EXENDTC, ~IMPUTATION,
    "SUBJ-001", "DRUG A", 1, "2023-01-01T08:00", "2023-01-14T08:00", "",
    "SUBJ-001", "DRUG A", 2, "2023-01-15T08:00", NA, "",
    "SUBJ-001", "DRUG A", 3, "2023-01-29T08:00", "2023-02-10T08:00", "",
    "SUBJ-001", "DRUG B", 1, "2023-01-01T10:00", "2023-01-14T10:00", "",
    "SUBJ-001", "DRUG B", 2, "2023-01-15T10:00", NA, "",
    "SUBJ-001", "DRUG B", 3, "2023-01-29T10:00", "2023-02-10T10:00", "",
    "SUBJ-002", "DRUG A", 1, "2023-01-01T09:00", "2023-01-14T09:00", "",
    "SUBJ-002", "DRUG A", 2, "2023-01-15T09:00", NA, "",
    "SUBJ-002", "DRUG A", 3, "2023-01-29T09:00", "2023-02-10T09:00", ""
  ) %>% lubrify_dates()

  # Capture output to avoid message cluttering test results
  result <- capture_messages(
    out <- impute_missing_exendtc(ex)
  )

  # Check imputations for SUBJ-001, DRUG A
  row_s1_da <- out[out$USUBJID == "SUBJ-001" & out$EXTRT == "DRUG A" & out$EXSEQ == 2, ]
  expect_equal(as.character(row_s1_da$EXENDTC), "2023-01-28 08:00:00")

  # Check imputations for SUBJ-001, DRUG B
  row_s1_db <- out[out$USUBJID == "SUBJ-001" & out$EXTRT == "DRUG B" & out$EXSEQ == 2, ]
  expect_equal(as.character(row_s1_db$EXENDTC), "2023-01-28 10:00:00")

  # Check imputations for SUBJ-002, DRUG A
  row_s2_da <- out[out$USUBJID == "SUBJ-002" & out$EXTRT == "DRUG A" & out$EXSEQ == 2, ]
  expect_equal(as.character(row_s2_da$EXENDTC), "2023-01-28 09:00:00")
})


test_that("impute_missing_exendtc preserves existing data and columns", {
  # Create test data with additional columns
  ex <- tibble::tribble(
    ~USUBJID, ~EXTRT, ~EXSEQ, ~EXSTDTC, ~EXENDTC, ~IMPUTATION, ~EXTRA_COL,
    "SUBJ-001", "DRUG A", 1, "2023-01-01T08:00", "2023-01-14T08:00", "", "A",
    "SUBJ-001", "DRUG A", 2, "2023-01-15T08:00", NA, "", "B",
    "SUBJ-001", "DRUG A", 3, "2023-01-29T08:00", "2023-02-10T08:00", "", "C"
  ) %>% lubrify_dates()

  # Capture output to avoid message cluttering test results
  result <- capture_messages(
    out <- impute_missing_exendtc(ex)
  )

  # Check that all original columns are preserved
  expect_true(all(names(ex) %in% names(out)))

  # Check that EXTRA_COL values are preserved
  expect_equal(out$EXTRA_COL, c("A", "B", "C"))

  # Check that non-imputed values remain unchanged
  expect_equal(out$EXENDTC[1], ex$EXENDTC[1])
  expect_equal(out$EXENDTC[3], ex$EXENDTC[3])
})
