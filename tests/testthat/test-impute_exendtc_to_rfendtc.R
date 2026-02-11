test_that("impute_exendtc_to_rfendtc validates input correctly", {
  # Test missing columns in DM
  dm_missing_cols <- data.frame(USUBJID = c("SUBJ-001", "SUBJ-002"))

  ex_valid <- tibble::tribble(
    ~USUBJID, ~EXTRT, ~EXSEQ, ~EXSTDTC, ~EXENDTC, ~IMPUTATION,
    "SUBJ-001", "DRUG A", 1, "2023-01-01T08:00", "2023-01-14T08:00", NA,
    "SUBJ-001", "DRUG A", 2, "2023-01-15T08:00", NA, NA,
    "SUBJ-002", "DRUG A", 1, "2023-01-01T08:00", "2023-01-14T08:00", NA
  ) %>% lubrify_dates()

  expect_message(
    impute_exendtc_to_rfendtc(ex_valid, dm_missing_cols, "DRUG A"),
    "Cannot impute missing EXENDTC in final administration episode"
  )

  # Test missing columns in EX
  dm_valid <- tibble::tribble(
    ~USUBJID, ~RFSTDTC, ~RFENDTC,
    "SUBJ-001", "2023-01-01T08:00", "2023-01-31T08:00",
    "SUBJ-002", "2023-01-01T08:00", "2023-01-31T08:00"
  ) %>% lubrify_dates()

  ex_missing_cols <- tibble::tribble(
    ~USUBJID, ~EXTRT,
    "SUBJ-001", "DRUG A",
    "SUBJ-002", "DRUG A"
  )

  expect_error(
    impute_exendtc_to_rfendtc(ex_missing_cols, dm_valid, "DRUG A"),
    "Missing columns in domain EX: EXSTDTC and EXENDTC"
  )
})


test_that("impute_exendtc_to_rfendtc adds IMPUTATION column if not present", {
  dm <- tibble::tribble(
    ~USUBJID, ~RFSTDTC, ~RFENDTC,
    "SUBJ-001", "2023-01-01T08:00", "2023-01-31T08:00",
    "SUBJ-002", "2023-01-01T08:00", "2023-01-31T08:00"
  )

  ex <- tibble::tribble(
    ~USUBJID, ~EXTRT, ~EXSEQ, ~EXSTDTC, ~EXENDTC,
    "SUBJ-001", "DRUG A", 1, "2023-01-01T08:00", "2023-01-14T08:00",
    "SUBJ-001", "DRUG A", 2, "2023-01-15T08:00", NA,
    "SUBJ-002", "DRUG A", 1, "2023-01-01T08:00", "2023-01-14T08:00"
  )

  result <- impute_exendtc_to_rfendtc(ex, dm, "DRUG A", silent = TRUE)

  expect_true("IMPUTATION" %in% names(result))
})


test_that("impute_exendtc_to_rfendtc performs imputations correctly", {
  # Create test data with two subjects, one with missing final EXENDTC
  dm <- tibble::tribble(
    ~USUBJID, ~RFSTDTC, ~RFENDTC,
    "SUBJ-001", "2023-01-01T08:00", "2023-01-31T08:00",
    "SUBJ-002", "2023-01-01T08:00", "2023-01-31T08:00",
    "SUBJ-003", "2023-01-01T08:00", "2023-01-31T08:00"
  ) %>% lubrify_dates()

  ex <- tibble::tribble(
    ~USUBJID, ~EXTRT, ~EXSEQ, ~EXSTDTC, ~EXENDTC, ~IMPUTATION,
    "SUBJ-001", "DRUG A", 1, "2023-01-01T08:00", "2023-01-14T08:00", NA,
    "SUBJ-001", "DRUG A", 2, "2023-01-15T08:00", NA, NA,
    "SUBJ-002", "DRUG A", 1, "2023-01-01T08:00", "2023-01-14T08:00", NA,
    "SUBJ-003", "DRUG B", 1, "2023-01-01T08:00", "2023-01-14T08:00", NA,
    "SUBJ-003", "DRUG B", 2, "2023-01-15T08:00", NA, NA
  ) %>% lubrify_dates()

  result <- impute_exendtc_to_rfendtc(ex, dm, "DRUG A", silent = TRUE)

  # Check that missing EXENDTC for SUBJ-001's last administration was imputed
  subj1_last_row <- result[result$USUBJID == "SUBJ-001" & result$EXSEQ == 2, ]
  expect_equal(as.character(subj1_last_row$EXENDTC), "2023-01-31 08:00:00")
  expect_equal(subj1_last_row$IMPUTATION, "missing EXENDTC set to RFENDTC")
})


test_that("impute_exendtc_to_rfendtc does not impute non-last administrations", {
  dm <- tibble::tribble(
    ~USUBJID, ~RFSTDTC, ~RFENDTC,
    "SUBJ-001", "2023-01-01T08:00", "2023-01-31T08:00"
  ) %>% lubrify_dates()

  ex <- tibble::tribble(
    ~USUBJID, ~EXTRT, ~EXSEQ, ~EXSTDTC, ~EXENDTC, ~IMPUTATION,
    "SUBJ-001", "DRUG A", 1, "2023-01-01T08:00", "2023-01-14T08:00", "",
    "SUBJ-001", "DRUG A", 2, "2023-01-15T08:00", NA, "",
    "SUBJ-001", "DRUG A", 3, "2023-01-29T08:00", NA, ""
  ) %>% lubrify_dates()

  result <- impute_exendtc_to_rfendtc(ex, dm, "DRUG A", silent = TRUE)

  # Check that middle administration with missing EXENDTC was not imputed
  middle_row <- result[result$USUBJID == "SUBJ-001" & result$EXSEQ == 2, ]
  expect_true(is.na(middle_row$EXENDTC))
  expect_equal(middle_row$IMPUTATION, "")

  # Check that last administration was imputed
  last_row <- result[result$USUBJID == "SUBJ-001" & result$EXSEQ == 3, ]
  expect_equal(as.character(last_row$EXENDTC), "2023-01-31 08:00:00")
  expect_equal(last_row$IMPUTATION, "missing EXENDTC set to RFENDTC")
})


test_that("impute_exendtc_to_rfendtc handles multiple treatments per subject correctly", {
  dm <- tibble::tribble(
    ~USUBJID, ~RFSTDTC, ~RFENDTC,
    "SUBJ-001", "2023-01-01T08:00", "2023-01-31T08:00"
  ) %>% lubrify_dates()

  ex <- tibble::tribble(
    ~USUBJID, ~EXTRT, ~EXSEQ, ~EXSTDTC, ~EXENDTC, ~IMPUTATION,
    "SUBJ-001", "DRUG A", 1, "2023-01-01T08:00", "2023-01-14T08:00", "",
    "SUBJ-001", "DRUG A", 2, "2023-01-15T08:00", NA, "",
    "SUBJ-001", "DRUG B", 1, "2023-01-05T08:00", "2023-01-19T08:00", "",
    "SUBJ-001", "DRUG B", 2, "2023-01-20T08:00", NA, ""
  ) %>% lubrify_dates()

  result <- impute_exendtc_to_rfendtc(ex, dm, "DRUG A", silent = TRUE)

  # Check that last administration of DRUG A was imputed
  drugA_last_row <- result[result$USUBJID == "SUBJ-001" & result$EXTRT == "DRUG A" & result$EXSEQ == 2, ]
  expect_equal(as.character(drugA_last_row$EXENDTC), "2023-01-31 08:00:00")
  expect_equal(drugA_last_row$IMPUTATION, "missing EXENDTC set to RFENDTC")
})


test_that("impute_exendtc_to_rfendtc returns unmodified data when no imputations needed", {
  dm <- tibble::tribble(
    ~USUBJID, ~RFSTDTC, ~RFENDTC,
    "SUBJ-001", "2023-01-01T08:00", "2023-01-31T08:00",
    "SUBJ-002", "2023-01-01T08:00", "2023-01-31T08:00"
  ) %>% lubrify_dates()

  ex <- tibble::tribble(
    ~USUBJID, ~EXTRT, ~EXSEQ, ~EXSTDTC, ~EXENDTC, ~IMPUTATION,
    "SUBJ-001", "DRUG A", 1, "2023-01-01T08:00", "2023-01-14T08:00", "",
    "SUBJ-001", "DRUG A", 2, "2023-01-15T08:00", "2023-01-30T08:00", "",
    "SUBJ-002", "DRUG A", 1, "2023-01-01T08:00", "2023-01-14T08:00", "",
    "SUBJ-002", "DRUG A", 2, "2023-01-15T08:00", "2023-01-30T08:00", ""
  ) %>% lubrify_dates()

  result <- impute_exendtc_to_rfendtc(ex, dm, "DRUG A")

  # Check that data remains unchanged when no imputations are needed
  expect_equal(result$EXENDTC, ex$EXENDTC)
  expect_equal(result$IMPUTATION, ex$IMPUTATION)
})


test_that("impute_exendtc_to_rfendtc handles case with no RFENDTC in DM", {
  dm <- tibble::tribble(
    ~USUBJID, ~RFSTDTC, ~RFENDTC,
    "SUBJ-001", "2023-01-01T08:00", "",
    "SUBJ-002", "2023-01-01T08:00", ""
  ) %>% lubrify_dates()

  ex <- tibble::tribble(
    ~USUBJID, ~EXTRT, ~EXSEQ, ~EXSTDTC, ~EXENDTC, ~IMPUTATION,
    "SUBJ-001", "DRUG A", 1, "2023-01-01T08:00", "2023-01-14T08:00", "",
    "SUBJ-001", "DRUG A", 2, "2023-01-15T08:00", NA, "",
    "SUBJ-002", "DRUG A", 1, "2023-01-01T08:00", "2023-01-14T08:00", ""
  ) %>% lubrify_dates()

  result <- impute_exendtc_to_rfendtc(ex, dm, "DRUG A")

  # Check that no imputations occurred when RFENDTC is NA
  expect_true(is.na(result$EXENDTC[2]))
  expect_equal(result$IMPUTATION[2], "")
})
