test_that("impute_admin_times_from_pcrftdtc works", {
  # Sample input data frames
  obj <- tribble(
    ~USUBJID,   ~ANALYTE, ~DTC,              ~IMPUTATION,
    "SUBJ-001", "Drug A", "2023-01-15",      "",
    "SUBJ-001", "Drug A", "2023-01-16",      "",
    "SUBJ-002", "Drug A", "2023-01-15",      "",
    "SUBJ-003", "Drug B", "2023-01-15T12:30", "",
    "SUBJ-003", "Drug B", "2023-01-16",      ""
  )

  pc <- tribble(
    ~USUBJID,   ~PCTESTCD,   ~PCRFTDTC,
    "SUBJ-001", "DRUGACONC", "2023-01-15T08:15",
    "SUBJ-001", "DRUGACONC", "2023-01-16T09:30",
    "SUBJ-002", "DRUGACONC", "2023-01-15T10:45",
    "SUBJ-003", "DRUGBCONC", "2023-01-15T12:30",
    "SUBJ-003", "DRUGBCONC", "2023-01-16T14:00"
  )

  # Expected output
  expected <- tribble(
    ~USUBJID,   ~ANALYTE, ~DTC,               ~IMPUTATION,
    "SUBJ-001", "Drug A", "2023-01-15T08:15", "admin time copied from PCRFTDTC",
    "SUBJ-001", "Drug A", "2023-01-16T09:30", "admin time copied from PCRFTDTC",
    "SUBJ-002", "Drug A", "2023-01-15T10:45", "admin time copied from PCRFTDTC",
    "SUBJ-003", "Drug B", "2023-01-15T12:30", "",
    "SUBJ-003", "Drug B", "2023-01-16T00:00", ""
  ) %>% lubrify_dates()

  # Test for Drug A
  result_a <- impute_admin_times_from_pcrftdtc(obj, pc, "Drug A", "DRUGACONC")
  expect_equal(result_a, expected)
})


test_that("impute_admin_times_from_pcrftdtc handles missing data", {
  # Sample input with missing reference times
  obj <- tribble(
    ~USUBJID, ~ANALYTE, ~DTC,          ~IMPUTATION,
    "SUBJ-001", "Drug A", "2023-01-15", "",
    "SUBJ-002", "Drug A", "2023-01-15", ""
  )

  pc <- tribble(
    ~USUBJID, ~PCTESTCD, ~PCRFTDTC,
    "SUBJ-001", "DRUGACONC", "2023-01-15T08:15",
    # No reference time for SUBJ-002
    "SUBJ-003", "DRUGACONC", "2023-01-16T09:30" # Different subject
  )

  # Expected output
  expected <- tribble(
    ~USUBJID, ~ANALYTE, ~DTC,                ~IMPUTATION,
    "SUBJ-001", "Drug A", "2023-01-15T08:15", "admin time copied from PCRFTDTC",
    "SUBJ-002", "Drug A", "2023-01-15",       ""
  ) %>% lubrify_dates()

  result <- impute_admin_times_from_pcrftdtc(obj, pc, "Drug A", "DRUGACONC")
  expect_equal(result, expected)
})


test_that("impute_admin_times_from_pcrftdtc handles date mismatches", {
  # Sample input with date mismatches
  obj <- tribble(
    ~USUBJID, ~ANALYTE, ~DTC,          ~IMPUTATION,
    "SUBJ-001", "Drug A", "2023-01-15", "",
    "SUBJ-001", "Drug A", "2023-01-16", ""
  )

  pc <- tribble(
    ~USUBJID, ~PCTESTCD, ~PCRFTDTC,
    "SUBJ-001", "DRUGACONC", "2023-01-15T08:15",
    "SUBJ-001", "DRUGACONC", "2023-01-17T09:30" # Date mismatch with second row
  )

  # Expected output
  expected <- tribble(
    ~USUBJID, ~ANALYTE, ~DTC,                ~IMPUTATION,
    "SUBJ-001", "Drug A", "2023-01-15T08:15", "admin time copied from PCRFTDTC",
    "SUBJ-001", "Drug A", "2023-01-16",       ""
  ) %>% lubrify_dates()

  result <- impute_admin_times_from_pcrftdtc(obj, pc, "Drug A", "DRUGACONC")
  expect_equal(result, expected)
})


test_that("impute_admin_times_from_pcrftdtc preserves existing times", {
  # Sample input with some existing times
  obj <- tribble(
      ~USUBJID, ~ANALYTE, ~DTC,                ~IMPUTATION,
    "SUBJ-001", "Drug A", "2023-01-15",       "",
    "SUBJ-001", "Drug A", "2023-01-16T10:00", "",
    "SUBJ-002", "Drug A", "2023-01-15",       ""
  )

  pc <- tribble(
      ~USUBJID, ~PCTESTCD, ~PCRFTDTC,
    "SUBJ-001", "DRUGACONC", "2023-01-15T08:15",
    "SUBJ-001", "DRUGACONC", "2023-01-16T09:30", # Different time than existing
    "SUBJ-002", "DRUGACONC", "2023-01-15T10:45"
  )

  # Expected output - the function should preserve the existing time (10:00)
  expected <- tibble::tribble(
      ~USUBJID, ~ANALYTE,                  ~DTC,                            ~IMPUTATION,
    "SUBJ-001", "Drug A", "2023-01-15 08:15:00",      "admin time copied from PCRFTDTC",
    "SUBJ-001", "Drug A", "2023-01-16 09:30:00", "admin time from PCRFTDTC prioritized",
    "SUBJ-002", "Drug A", "2023-01-15 10:45:00",      "admin time copied from PCRFTDTC"
  ) %>% lubrify_dates()

  expect_message(
    result <- impute_admin_times_from_pcrftdtc(obj, pc, "Drug A", "DRUGACONC",
                                               silent = FALSE),
    "Analyte Drug A: Conflicting administration time and PCRFTDTC")
  expect_equal(result, expected)
})
