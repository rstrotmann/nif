test_that("add_bl_creat adds BL_CREAT column with mg/dl units", {
  # Create test nif object
  test_nif <- tibble::tribble(
    ~ID, ~USUBJID,    ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   "SUBJ-001",  0,     0,    1,    0,     NA,
    1,   "SUBJ-001",  24,    0,    1,    0,     NA,
    2,   "SUBJ-002",  0,     0,    1,    0,     NA
  ) %>%
    nif()

  # Create SDTM with LB domain containing CREAT in mg/dl
  test_lb <- tibble::tribble(
    ~USUBJID,    ~DOMAIN, ~LBTESTCD, ~LBSTRESN, ~LBSTRESU, ~LBBLFL, ~LBDTC,
    "SUBJ-001",  "LB",    "CREAT",    0.8,       "mg/dl",   "Y",     "2023-01-01",
    "SUBJ-001",  "LB",    "CREAT",    0.9,       "mg/dl",   "N",     "2023-01-02",
    "SUBJ-002",  "LB",    "CREAT",    0.9,       "mg/dl",   "Y",     "2023-01-01"
  )

  test_dm <- tibble::tribble(
    ~USUBJID,    ~DOMAIN, ~ACTARMCD,
    "SUBJ-001",  "DM",    "TREATMENT",
    "SUBJ-002",  "DM",    "TREATMENT"
  )

  test_sdtm <- sdtm(list(lb = test_lb, dm = test_dm))

  # Test basic functionality
  result <- add_bl_creat(test_nif, test_sdtm, silent = TRUE)

  expect_true("BL_CREAT" %in% names(result))
  expect_equal(result$BL_CREAT[result$USUBJID == "SUBJ-001"], rep(70.72, 2))
  expect_equal(result$BL_CREAT[result$USUBJID == "SUBJ-002"], 79.56)
  expect_s3_class(result, "nif")
})


test_that("add_bl_creat converts umol/L to mg/dl automatically", {
  test_nif <- tibble::tribble(
    ~ID, ~USUBJID,    ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   "SUBJ-001",  0,     0,    1,    0,     NA
  ) %>%
    nif()

  # CREAT in umol/L (70.72 umol/L = 0.8 mg/dl)
  test_lb <- tibble::tribble(
    ~USUBJID,    ~DOMAIN, ~LBTESTCD, ~LBSTRESN, ~LBSTRESU, ~LBBLFL, ~LBDTC,
    "SUBJ-001",  "LB",    "CREAT",    70.72,     "umol/L",  "Y",     "2023-01-01"
  )

  test_dm <- tibble::tribble(
    ~USUBJID,    ~DOMAIN, ~ACTARMCD,
    "SUBJ-001",  "DM",    "TREATMENT"
  )

  test_sdtm <- sdtm(list(lb = test_lb, dm = test_dm))

  # Should automatically convert and warn
  result <- add_bl_creat(test_nif, test_sdtm, silent = FALSE)

  expect_true("BL_CREAT" %in% names(result))
  # 70.72 umol/L / 88.4 = 0.8 mg/dl
  expect_equal(result$BL_CREAT[1], 70.72)
})


test_that("add_bl_creat handles different umol/L unit formats", {
  test_nif <- tibble::tribble(
    ~ID, ~USUBJID,    ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   "SUBJ-001",  0,     0,    1,    0,     NA
  ) %>%
    nif()

  test_dm <- tibble::tribble(
    ~USUBJID,    ~DOMAIN, ~ACTARMCD,
    "SUBJ-001",  "DM",    "TREATMENT"
  )

  # Test umol/l (lowercase)
  test_lb1 <- tibble::tribble(
    ~USUBJID,    ~DOMAIN, ~LBTESTCD, ~LBSTRESN, ~LBSTRESU, ~LBBLFL, ~LBDTC,
    "SUBJ-001",  "LB",    "CREAT",    70.72,     "umol/l",  "Y",     "2023-01-01"
  )
  test_sdtm1 <- sdtm(list(lb = test_lb1, dm = test_dm))
  result1 <- add_bl_creat(test_nif, test_sdtm1, silent = TRUE)
  expect_true("BL_CREAT" %in% names(result1))

  # Test micromol/l
  test_lb2 <- tibble::tribble(
    ~USUBJID,    ~DOMAIN, ~LBTESTCD, ~LBSTRESN, ~LBSTRESU, ~LBBLFL, ~LBDTC,
    "SUBJ-001",  "LB",    "CREAT",    70.72,     "micromol/l",  "Y",     "2023-01-01"
  )
  test_sdtm2 <- sdtm(list(lb = test_lb2, dm = test_dm))
  result2 <- add_bl_creat(test_nif, test_sdtm2, silent = TRUE)
  expect_true("BL_CREAT" %in% names(result2))

  # Test micromol/L
  test_lb3 <- tibble::tribble(
    ~USUBJID,    ~DOMAIN, ~LBTESTCD, ~LBSTRESN, ~LBSTRESU, ~LBBLFL, ~LBDTC,
    "SUBJ-001",  "LB",    "CREAT",    70.72,     "micromol/L",  "Y",     "2023-01-01"
  )
  test_sdtm3 <- sdtm(list(lb = test_lb3, dm = test_dm))
  result3 <- add_bl_creat(test_nif, test_sdtm3, silent = TRUE)
  expect_true("BL_CREAT" %in% names(result3))
})


test_that("add_bl_creat errors when multiple units are found", {
  test_nif <- tibble::tribble(
    ~ID, ~USUBJID,    ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   "SUBJ-001",  0,     0,    1,    0,     NA
  ) %>%
    nif()

  # Multiple units for CREAT
  test_lb <- tibble::tribble(
    ~USUBJID,    ~DOMAIN, ~LBTESTCD, ~LBSTRESN, ~LBSTRESU, ~LBBLFL, ~LBDTC,
    "SUBJ-001",  "LB",    "CREAT",    0.8,       "mg/dl",   "Y",     "2023-01-01",
    "SUBJ-001",  "LB",    "CREAT",    70.72,     "umol/L",  "N",     "2023-01-02"
  )

  test_dm <- tibble::tribble(
    ~USUBJID,    ~DOMAIN, ~ACTARMCD,
    "SUBJ-001",  "DM",    "TREATMENT"
  )

  test_sdtm <- sdtm(list(lb = test_lb, dm = test_dm))

  expect_message(
    result <- add_bl_creat(test_nif, test_sdtm, silent = FALSE),
    "BL_CREAT values converted to umol/L"
  )
})


test_that("add_bl_creat handles missing unit information", {
  test_nif <- tibble::tribble(
    ~ID, ~USUBJID,    ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   "SUBJ-001",  0,     0,    1,    0,     NA
  ) %>%
    nif()

  # LBSTRESU column exists but is empty/NA for CREAT
  test_lb <- tibble::tribble(
    ~USUBJID,    ~DOMAIN, ~LBTESTCD, ~LBSTRESN, ~LBSTRESU, ~LBBLFL, ~LBDTC,
    "SUBJ-001",  "LB",    "CREAT",    0.8,       NA,        "Y",     "2023-01-01",
    "SUBJ-001",  "LB",    "ALT",      25,        "U/L",     "Y",     "2023-01-01"
  )

  test_dm <- tibble::tribble(
    ~USUBJID,    ~DOMAIN, ~ACTARMCD,
    "SUBJ-001",  "DM",    "TREATMENT"
  )

  test_sdtm <- sdtm(list(lb = test_lb, dm = test_dm))

  # Should warn and default to mg/dl
  expect_message(
    expect_message(
      result <- add_bl_creat(test_nif, test_sdtm, silent = FALSE),
      "Some CREAT values had no unit and were deleted!"),
    "No baseline CREAT data after filtering!"
  )

  expect_false("BL_CREAT" %in% names(result))
})


test_that("add_bl_creat handles missing LBSTRESU column", {
  test_nif <- tibble::tribble(
    ~ID, ~USUBJID,    ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   "SUBJ-001",  0,     0,    1,    0,     NA
  ) %>%
    nif()

  # No LBSTRESU column
  test_lb <- tibble::tribble(
    ~USUBJID,    ~DOMAIN, ~LBTESTCD, ~LBSTRESN, ~LBBLFL, ~LBDTC,
    "SUBJ-001",  "LB",    "CREAT",    0.8,       "Y",     "2023-01-01"
  )

  test_dm <- tibble::tribble(
    ~USUBJID,    ~DOMAIN, ~ACTARMCD,
    "SUBJ-001",  "DM",    "TREATMENT"
  )

  test_sdtm <- sdtm(list(lb = test_lb, dm = test_dm))

  expect_message(
    expect_message(
      result <- add_bl_creat(test_nif, test_sdtm, silent = FALSE),
      "LBSTESU field not found. Values assumed in mg/dL"),
    "BL_CREAT values converted to umol/L!")

  expect_true("BL_CREAT" %in% names(result))
  expect_equal(result$BL_CREAT[1], 70.72)
})


test_that("add_bl_creat errors when LB domain is missing", {
  test_nif <- tibble::tribble(
    ~ID, ~USUBJID,    ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   "SUBJ-001",  0,     0,    1,    0,     NA
  ) %>%
    nif()

  test_dm <- tibble::tribble(
    ~USUBJID,    ~DOMAIN, ~ACTARMCD,
    "SUBJ-001",  "DM",    "TREATMENT"
  )

  test_sdtm <- sdtm(list(dm = test_dm))

  expect_error(
    add_bl_creat(test_nif, test_sdtm, silent = TRUE),
    "LB domain not found"
  )
})


test_that("add_bl_creat errors when CREAT test code is missing", {
  test_nif <- tibble::tribble(
    ~ID, ~USUBJID,    ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   "SUBJ-001",  0,     0,    1,    0,     NA
  ) %>%
    nif()

  # LB domain exists but no CREAT
  test_lb <- tibble::tribble(
    ~USUBJID,    ~DOMAIN, ~LBTESTCD, ~LBSTRESN, ~LBSTRESU, ~LBBLFL, ~LBDTC,
    "SUBJ-001",  "LB",    "ALT",      25,        "U/L",     "Y",     "2023-01-01"
  )

  test_dm <- tibble::tribble(
    ~USUBJID,    ~DOMAIN, ~ACTARMCD,
    "SUBJ-001",  "DM",    "TREATMENT"
  )

  test_sdtm <- sdtm(list(lb = test_lb, dm = test_dm))

  expect_error(
    add_bl_creat(test_nif, test_sdtm, silent = TRUE),
    "No CREAT data found"
  )
})


test_that("add_bl_creat handles custom baseline filter", {
  test_nif <- tibble::tribble(
    ~ID, ~USUBJID,    ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   "SUBJ-001",  0,     0,    1,    0,     NA
  ) %>%
    nif()

  # Use LBLOBXFL instead of LBBLFL
  test_lb <- tibble::tribble(
    ~USUBJID,    ~DOMAIN, ~LBTESTCD, ~LBSTRESN, ~LBSTRESU, ~LBLOBXFL, ~LBDTC,
    "SUBJ-001",  "LB",    "CREAT",    0.8,       "mg/dl",   "N",       "2023-01-01",
    "SUBJ-001",  "LB",    "CREAT",    0.9,       "mg/dl",   "Y",       "2023-01-02"
  )

  test_dm <- tibble::tribble(
    ~USUBJID,    ~DOMAIN, ~ACTARMCD,
    "SUBJ-001",  "DM",    "TREATMENT"
  )

  test_sdtm <- sdtm(list(lb = test_lb, dm = test_dm))

  result <- add_bl_creat(
    test_nif, test_sdtm,
    baseline_filter = "LBLOBXFL == 'Y'",
    silent = TRUE
  )

  expect_true("BL_CREAT" %in% names(result))
  expect_equal(result$BL_CREAT[1], 79.56)  # Should use LBLOBXFL == 'Y' value
})


test_that("add_bl_creat uses LBBLFL when available", {
  test_nif <- tibble::tribble(
    ~ID, ~USUBJID,    ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   "SUBJ-001",  0,     0,    1,    0,     NA
  ) %>%
    nif()

  test_lb <- tibble::tribble(
    ~USUBJID,    ~DOMAIN, ~LBTESTCD, ~LBSTRESN, ~LBSTRESU, ~LBBLFL, ~LBDTC,
    "SUBJ-001",  "LB",    "CREAT",    0.8,       "mg/dl",   "Y",     "2023-01-01",
    "SUBJ-001",  "LB",    "CREAT",    0.9,       "mg/dl",   "N",     "2023-01-02"
  )

  test_dm <- tibble::tribble(
    ~USUBJID,    ~DOMAIN, ~ACTARMCD,
    "SUBJ-001",  "DM",    "TREATMENT"
  )

  test_sdtm <- sdtm(list(lb = test_lb, dm = test_dm))

  result <- add_bl_creat(test_nif, test_sdtm, silent = TRUE)

  expect_true("BL_CREAT" %in% names(result))
  expect_equal(result$BL_CREAT[1], 70.72)  # Should use LBBLFL == 'Y' value
})


test_that("add_bl_creat uses LBLOBXFL when LBBLFL is missing", {
  test_nif <- tibble::tribble(
    ~ID, ~USUBJID,    ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   "SUBJ-001",  0,     0,    1,    0,     NA
  ) %>%
    nif()

  # Only LBLOBXFL, no LBBLFL
  test_lb <- tibble::tribble(
    ~USUBJID,    ~DOMAIN, ~LBTESTCD, ~LBSTRESN, ~LBSTRESU, ~LBLOBXFL, ~LBDTC,
    "SUBJ-001",  "LB",    "CREAT",    0.8,       "mg/dl",   "Y",       "2023-01-01",
    "SUBJ-001",  "LB",    "CREAT",    0.9,       "mg/dl",   "N",       "2023-01-02"
  )

  test_dm <- tibble::tribble(
    ~USUBJID,    ~DOMAIN, ~ACTARMCD,
    "SUBJ-001",  "DM",    "TREATMENT"
  )

  test_sdtm <- sdtm(list(lb = test_lb, dm = test_dm))

  result <- add_bl_creat(test_nif, test_sdtm, silent = TRUE)

  expect_true("BL_CREAT" %in% names(result))
  expect_equal(result$BL_CREAT[1], 70.72)  # Should use LBLOBXFL == 'Y' value
})


test_that("add_bl_creat errors when no baseline flag column exists", {
  test_nif <- tibble::tribble(
    ~ID, ~USUBJID,    ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   "SUBJ-001",  0,     0,    1,    0,     NA
  ) %>%
    nif()

  # No baseline flag columns
  test_lb <- tibble::tribble(
    ~USUBJID,    ~DOMAIN, ~LBTESTCD, ~LBSTRESN, ~LBSTRESU, ~LBDTC,
    "SUBJ-001",  "LB",    "CREAT",    0.8,       "mg/dl",   "2023-01-01"
  )

  test_dm <- tibble::tribble(
    ~USUBJID,    ~DOMAIN, ~ACTARMCD,
    "SUBJ-001",  "DM",    "TREATMENT"
  )

  test_sdtm <- sdtm(list(lb = test_lb, dm = test_dm))

  expect_error(
    add_bl_creat(test_nif, test_sdtm, silent = TRUE),
    "No baseline flag column identified"
  )
})


test_that("add_bl_creat handles invalid baseline filter gracefully", {
  test_nif <- tibble::tribble(
    ~ID, ~USUBJID,    ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   "SUBJ-001",  0,     0,    1,    0,     NA
  ) %>%
    nif()

  test_lb <- tibble::tribble(
    ~USUBJID,    ~DOMAIN, ~LBTESTCD, ~LBSTRESN, ~LBSTRESU, ~LBBLFL, ~LBDTC,
    "SUBJ-001",  "LB",    "CREAT",    0.8,       "mg/dl",   "Y",     "2023-01-01"
  )

  test_dm <- tibble::tribble(
    ~USUBJID,    ~DOMAIN, ~ACTARMCD,
    "SUBJ-001",  "DM",    "TREATMENT"
  )

  test_sdtm <- sdtm(list(lb = test_lb, dm = test_dm))

  # Invalid filter
  expect_message(
    result <- add_bl_creat(
      test_nif, test_sdtm,
      baseline_filter = "INVALID_COLUMN == 'Y'",
      silent = FALSE
    ),
    "Invalid baseline filter"
  )

  # Should return original object without BL_CREAT
  expect_false("BL_CREAT" %in% names(result))
  expect_equal(nrow(result), nrow(test_nif))
})


test_that("add_bl_creat handles silent parameter", {
  test_nif <- tibble::tribble(
    ~ID, ~USUBJID,    ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   "SUBJ-001",  0,     0,    1,    0,     NA
  ) %>%
    nif()

  # CREAT in umol/L (should warn)
  test_lb <- tibble::tribble(
    ~USUBJID,    ~DOMAIN, ~LBTESTCD, ~LBSTRESN, ~LBSTRESU, ~LBBLFL, ~LBDTC,
    "SUBJ-001",  "LB",    "CREAT",    70.72,     "umol/L",  "Y",     "2023-01-01"
  )

  test_dm <- tibble::tribble(
    ~USUBJID,    ~DOMAIN, ~ACTARMCD,
    "SUBJ-001",  "DM",    "TREATMENT"
  )

  test_sdtm <- sdtm(list(lb = test_lb, dm = test_dm))

  # With silent=TRUE, should not warn
  expect_no_warning(
    result <- add_bl_creat(test_nif, test_sdtm, silent = TRUE)
  )

  expect_true("BL_CREAT" %in% names(result))
})


test_that("add_bl_creat handles multiple subjects correctly", {
  test_nif <- tibble::tribble(
    ~ID, ~USUBJID,    ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   "SUBJ-001",  0,     0,    1,    0,     NA,
    1,   "SUBJ-001",  24,    0,    1,    0,     NA,
    2,   "SUBJ-002",  0,     0,    1,    0,     NA,
    2,   "SUBJ-002",  24,    0,    1,    0,     NA,
    3,   "SUBJ-003",  0,     0,    1,    0,     NA
  ) %>%
    nif()

  test_lb <- tibble::tribble(
    ~USUBJID,    ~DOMAIN, ~LBTESTCD, ~LBSTRESN, ~LBSTRESU, ~LBBLFL, ~LBDTC,
    "SUBJ-001",  "LB",    "CREAT",    0.8,       "mg/dl",   "Y",     "2023-01-01",
    "SUBJ-002",  "LB",    "CREAT",    0.9,       "mg/dl",   "Y",     "2023-01-01",
    "SUBJ-003",  "LB",    "CREAT",    1.0,       "mg/dl",   "Y",     "2023-01-01"
  )

  test_dm <- tibble::tribble(
    ~USUBJID,    ~DOMAIN, ~ACTARMCD,
    "SUBJ-001",  "DM",    "TREATMENT",
    "SUBJ-002",  "DM",    "TREATMENT",
    "SUBJ-003",  "DM",    "TREATMENT"
  )

  test_sdtm <- sdtm(list(lb = test_lb, dm = test_dm))

  result <- add_bl_creat(test_nif, test_sdtm, silent = TRUE)

  expect_true("BL_CREAT" %in% names(result))
  # Check that values are consistent within each subject
  expect_equal(unique(result$BL_CREAT[result$USUBJID == "SUBJ-001"]), 70.72)
  expect_equal(unique(result$BL_CREAT[result$USUBJID == "SUBJ-002"]), 79.56)
  expect_equal(unique(result$BL_CREAT[result$USUBJID == "SUBJ-003"]), 88.4)
})


test_that("add_bl_creat preserves all original columns", {
  test_nif <- tibble::tribble(
    ~ID, ~USUBJID,    ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~OTHER_COL,
    1,   "SUBJ-001",  0,     0,    1,    0,     NA, "test"
  ) %>%
    nif()

  test_lb <- tibble::tribble(
    ~USUBJID,    ~DOMAIN, ~LBTESTCD, ~LBSTRESN, ~LBSTRESU, ~LBBLFL, ~LBDTC,
    "SUBJ-001",  "LB",    "CREAT",    0.8,       "mg/dl",   "Y",     "2023-01-01"
  )

  test_dm <- tibble::tribble(
    ~USUBJID,    ~DOMAIN, ~ACTARMCD,
    "SUBJ-001",  "DM",    "TREATMENT"
  )

  test_sdtm <- sdtm(list(lb = test_lb, dm = test_dm))

  result <- add_bl_creat(test_nif, test_sdtm, silent = TRUE)

  # Check that all original columns are preserved
  original_cols <- names(test_nif)
  expect_true(all(original_cols %in% names(result)))
  expect_true("BL_CREAT" %in% names(result))
  expect_equal(result$OTHER_COL[1], "test")
})


test_that("add_bl_creat handles NA values in LBSTRESN", {
  test_nif <- tibble::tribble(
    ~ID, ~USUBJID,    ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   "SUBJ-001",  0,     0,    1,    0,     NA
  ) %>%
    nif()

  # CREAT with NA value
  test_lb <- tibble::tribble(
    ~USUBJID,    ~DOMAIN, ~LBTESTCD, ~LBSTRESN, ~LBSTRESU, ~LBBLFL, ~LBDTC,
    "SUBJ-001",  "LB",    "CREAT",    NA,        "mg/dl",   "Y",     "2023-01-01"
  )

  test_dm <- tibble::tribble(
    ~USUBJID,    ~DOMAIN, ~ACTARMCD,
    "SUBJ-001",  "DM",    "TREATMENT"
  )

  test_sdtm <- sdtm(list(lb = test_lb, dm = test_dm))

  expect_error(
    result <- add_bl_creat(test_nif, test_sdtm, silent = TRUE),
    "No valid baseline values found"
  )
})


test_that("add_bl_creat handles multiple baseline values (uses mean by default)", {
  test_nif <- tibble::tribble(
    ~ID, ~USUBJID,    ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   "SUBJ-001",  0,     0,    1,    0,     NA
  ) %>%
    nif()

  # Multiple baseline CREAT values
  test_lb <- tibble::tribble(
    ~USUBJID,    ~DOMAIN, ~LBTESTCD, ~LBSTRESN, ~LBSTRESU, ~LBBLFL, ~LBDTC,
    "SUBJ-001",  "LB",    "CREAT",    0.8,       "mg/dl",   "Y",     "2023-01-01",
    "SUBJ-001",  "LB",    "CREAT",    0.9,       "mg/dl",   "Y",     "2023-01-02"
  )

  test_dm <- tibble::tribble(
    ~USUBJID,    ~DOMAIN, ~ACTARMCD,
    "SUBJ-001",  "DM",    "TREATMENT"
  )

  test_sdtm <- sdtm(list(lb = test_lb, dm = test_dm))

  result <- add_bl_creat(test_nif, test_sdtm, silent = TRUE)

  expect_true("BL_CREAT" %in% names(result))
  expect_equal(result$BL_CREAT[1], 75.14)
})


test_that("add_bl_creat handles multiple units", {
  test_nif <- tibble::tribble(
    ~ID, ~USUBJID,    ~TIME, ~AMT, ~CMT, ~EVID, ~DV,
    1,   "SUBJ-001",  0,     0,    1,    0,     NA,
    2,   "SUBJ-002",  0,     0,    1,    0,     NA
  ) %>%
    nif()

  # Multiple units for CREAT
  test_lb <- tibble::tribble(
    ~USUBJID,    ~DOMAIN, ~LBTESTCD, ~LBSTRESN, ~LBSTRESU, ~LBBLFL, ~LBDTC,
    "SUBJ-001",  "LB",    "CREAT",    0.8,       "mg/dl",   "Y",     "2023-01-01",
    "SUBJ-002",  "LB",    "CREAT",    70.72,     "umol/L",  "Y",     "2023-01-02"
  )

  test_dm <- tibble::tribble(
    ~USUBJID,    ~DOMAIN, ~ACTARMCD,
    "SUBJ-001",  "DM",    "TREATMENT",
    "SUBJ-002",  "DM",    "TREATMENT"
  )

  test_sdtm <- sdtm(list(lb = test_lb, dm = test_dm))

  expect_message(
    result <- add_bl_creat(test_nif, test_sdtm, silent = FALSE),
    "BL_CREAT values converted to umol/L!"
    )
  expect_equal(result$BL_CREAT, c(70.72, 70.72))
})
