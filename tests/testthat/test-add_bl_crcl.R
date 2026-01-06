test_that("add_bl_crcl adds BL_CRCL column with default method (egfr_cg)", {
  # Create test data with all required columns
  test_nif <- tibble::tribble(
    ~ID, ~USUBJID,    ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~BL_CREAT, ~AGE, ~SEX, ~RACE,     ~WEIGHT,
    1,   "SUBJ-001",  0,     0,    1,    0,     NA,  0.8,       45,   0,    "WHITE",  70,
    1,   "SUBJ-001",  24,    0,    1,    0,     NA,  0.8,       45,   0,    "WHITE",  70,
    2,   "SUBJ-002",  0,     0,    1,    0,     NA,  0.9,       55,   1,    "WHITE",  65,
    2,   "SUBJ-002",  24,    0,    1,    0,     NA,  0.9,       55,   1,    "WHITE",  65
  ) %>%
    nif()

  result <- add_bl_crcl(test_nif)

  # Check that BL_CRCL column was added
  expect_true("BL_CRCL" %in% names(result))

  # Check that values are numeric
  expect_type(result$BL_CRCL, "double")

  # Check that values are calculated (not all NA)
  expect_false(all(is.na(result$BL_CRCL)))

  # Check that values are consistent within subjects
  subj1_crcl <- result$BL_CRCL[result$ID == 1]
  subj2_crcl <- result$BL_CRCL[result$ID == 2]
  expect_equal(length(unique(subj1_crcl)), 1)
  expect_equal(length(unique(subj2_crcl)), 1)

  # Check that nif class is preserved
  expect_s3_class(result, "nif")
})


test_that("add_bl_crcl works with egfr_cg method", {
  test_nif <- tibble::tribble(
    ~ID, ~USUBJID,    ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~BL_CREAT, ~AGE, ~SEX, ~RACE,     ~WEIGHT,
    1,   "SUBJ-001",  0,     0,    1,    0,     NA,  0.8,       45,   0,    "WHITE",  70
  ) %>%
    nif()

  result <- add_bl_crcl(test_nif, method = egfr_cg)

  expect_true("BL_CRCL" %in% names(result))
  expect_false(is.na(result$BL_CRCL[1]))
  expect_s3_class(result, "nif")
})


test_that("add_bl_crcl works with egfr_mdrd method", {
  test_nif <- tibble::tribble(
    ~ID, ~USUBJID,    ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~BL_CREAT, ~AGE, ~SEX, ~RACE,     ~WEIGHT,
    1,   "SUBJ-001",  0,     0,    1,    0,     NA,  0.8,       45,   0,    "WHITE",  70,
    2,   "SUBJ-002",  0,     0,    1,    0,     NA,  0.9,       55,   1,    "BLACK",  65
  ) %>%
    nif()

  result <- add_bl_crcl(test_nif, method = egfr_mdrd)

  expect_true("BL_CRCL" %in% names(result))
  expect_false(any(is.na(result$BL_CRCL)))
  expect_s3_class(result, "nif")

  # Check that different subjects have different values
  expect_true(result$BL_CRCL[result$ID == 1] != result$BL_CRCL[result$ID == 2])
})


test_that("add_bl_crcl works with egfr_raynaud method", {
  test_nif <- tibble::tribble(
    ~ID, ~USUBJID,    ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~BL_CREAT, ~AGE, ~SEX, ~RACE,     ~WEIGHT,
    1,   "SUBJ-001",  0,     0,    1,    0,     NA,  0.8,       45,   0,    "WHITE",  70
  ) %>%
    nif()

  result <- add_bl_crcl(test_nif, method = egfr_raynaud)

  expect_true("BL_CRCL" %in% names(result))
  expect_false(is.na(result$BL_CRCL[1]))
  expect_s3_class(result, "nif")
})


test_that("add_bl_crcl stops when BL_CREAT is missing", {
  test_nif <- tibble::tribble(
    ~ID, ~USUBJID,    ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~AGE, ~SEX, ~RACE,     ~WEIGHT,
    1,   "SUBJ-001",  0,     0,    1,    0,     NA,  45,   0,    "WHITE",  70
  ) %>%
    nif()

  expect_error(
    result <- add_bl_crcl(test_nif),
    "Missing coluns: BL_CREAT!"
  )
})


test_that("add_bl_crcl errors when required columns are missing", {
  # Missing AGE
  test_nif <- tibble::tribble(
    ~ID, ~USUBJID,    ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~BL_CREAT, ~SEX, ~RACE,     ~WEIGHT,
    1,   "SUBJ-001",  0,     0,    1,    0,     NA,  0.8,       0,    "WHITE",  70
  ) %>%
    nif()

  expect_error(
    add_bl_crcl(test_nif),
    "Missing coluns"
  )

  # Missing SEX
  test_nif2 <- tibble::tribble(
    ~ID, ~USUBJID,    ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~BL_CREAT, ~AGE, ~RACE,     ~WEIGHT,
    1,   "SUBJ-001",  0,     0,    1,    0,     NA,  0.8,       45,   "WHITE",  70
  ) %>%
    nif()

  expect_error(
    add_bl_crcl(test_nif2),
    "Missing coluns"
  )

  # Missing RACE
  test_nif3 <- tibble::tribble(
    ~ID, ~USUBJID,    ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~BL_CREAT, ~AGE, ~SEX, ~WEIGHT,
    1,   "SUBJ-001",  0,     0,    1,    0,     NA,  0.8,       45,   0,    70
  ) %>%
    nif()

  expect_error(
    add_bl_crcl(test_nif3),
    "Missing coluns"
  )

  # Missing WEIGHT
  test_nif4 <- tibble::tribble(
    ~ID, ~USUBJID,    ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~BL_CREAT, ~AGE, ~SEX, ~RACE,
    1,   "SUBJ-001",  0,     0,    1,    0,     NA,  0.8,       45,   0,    "WHITE"
  ) %>%
    nif()

  expect_error(
    add_bl_crcl(test_nif4),
    "Missing coluns"
  )
})


test_that("add_bl_crcl handles different sex encodings", {
  # Test with numeric sex (0 = male, 1 = female)
  test_nif_male <- tibble::tribble(
    ~ID, ~USUBJID,    ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~BL_CREAT, ~AGE, ~SEX, ~RACE,     ~WEIGHT,
    1,   "SUBJ-001",  0,     0,    1,    0,     NA,  0.8,       45,   0,    "WHITE",  70
  ) %>%
    nif()

  result_male <- add_bl_crcl(test_nif_male, method = egfr_cg)

  test_nif_female <- tibble::tribble(
    ~ID, ~USUBJID,    ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~BL_CREAT, ~AGE, ~SEX, ~RACE,     ~WEIGHT,
    1,   "SUBJ-001",  0,     0,    1,    0,     NA,  0.8,       45,   1,    "WHITE",  70
  ) %>%
    nif()

  result_female <- add_bl_crcl(test_nif_female, method = egfr_cg)

  # Female should have different CRCL than male (due to 0.85 factor in CG)
  expect_true(result_male$BL_CRCL[1] != result_female$BL_CRCL[1])
})


test_that("add_bl_crcl handles different race values", {
  # Test with BLACK race (affects MDRD calculation)
  test_nif_black <- tibble::tribble(
    ~ID, ~USUBJID,    ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~BL_CREAT, ~AGE, ~SEX, ~RACE,     ~WEIGHT,
    1,   "SUBJ-001",  0,     0,    1,    0,     NA,  0.8,       45,   0,    "BLACK",  70
  ) %>%
    nif()

  result_black <- add_bl_crcl(test_nif_black, method = egfr_mdrd)

  test_nif_white <- tibble::tribble(
    ~ID, ~USUBJID,    ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~BL_CREAT, ~AGE, ~SEX, ~RACE,     ~WEIGHT,
    1,   "SUBJ-001",  0,     0,    1,    0,     NA,  0.8,       45,   0,    "WHITE",  70
  ) %>%
    nif()

  result_white <- add_bl_crcl(test_nif_white, method = egfr_mdrd)

  # BLACK should have higher CRCL than WHITE (due to 1.212 factor in MDRD)
  expect_true(result_black$BL_CRCL[1] > result_white$BL_CRCL[1])
})


test_that("add_bl_crcl handles multiple subjects correctly", {
  test_nif <- tibble::tribble(
    ~ID, ~USUBJID,    ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~BL_CREAT, ~AGE, ~SEX, ~RACE,     ~WEIGHT,
    1,   "SUBJ-001",  0,     0,    1,    0,     NA,  0.8,       45,   0,    "WHITE",  70,
    1,   "SUBJ-001",  24,    0,    1,    0,     NA,  0.8,       45,   0,    "WHITE",  70,
    2,   "SUBJ-002",  0,     0,    1,    0,     NA,  0.9,       55,   1,    "WHITE",  65,
    2,   "SUBJ-002",  24,    0,    1,    0,     NA,  0.9,       55,   1,    "WHITE",  65,
    3,   "SUBJ-003",  0,     0,    1,    0,     NA,  1.0,       65,   0,    "BLACK",  80
  ) %>%
    nif()

  result <- add_bl_crcl(test_nif)

  # Check that BL_CRCL is consistent within each subject
  subj1_crcl <- unique(result$BL_CRCL[result$ID == 1])
  subj2_crcl <- unique(result$BL_CRCL[result$ID == 2])
  subj3_crcl <- unique(result$BL_CRCL[result$ID == 3])

  expect_length(subj1_crcl, 1)
  expect_length(subj2_crcl, 1)
  expect_length(subj3_crcl, 1)

  # Check that different subjects have different values (likely)
  expect_true(subj1_crcl != subj2_crcl || subj1_crcl != subj3_crcl)
})


test_that("add_bl_crcl handles NA values in input columns", {
  # Test with NA in BL_CREAT
  test_nif <- tibble::tribble(
    ~ID, ~USUBJID,    ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~BL_CREAT, ~AGE, ~SEX, ~RACE,     ~WEIGHT,
    1,   "SUBJ-001",  0,     0,    1,    0,     NA,  NA,        45,   0,    "WHITE",  70
  ) %>%
    nif()

  result <- add_bl_crcl(test_nif)

  # Should still add BL_CRCL column, but value may be NA or calculated
  expect_true("BL_CRCL" %in% names(result))
  expect_s3_class(result, "nif")
})


test_that("add_bl_crcl preserves all original columns", {
  test_nif <- tibble::tribble(
    ~ID, ~USUBJID,    ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~BL_CREAT, ~AGE, ~SEX, ~RACE,     ~WEIGHT, ~OTHER_COL,
    1,   "SUBJ-001",  0,     0,    1,    0,     NA,  0.8,       45,   0,    "WHITE",  70,       "test"
  ) %>%
    nif()

  result <- add_bl_crcl(test_nif)

  # Check that all original columns are preserved
  original_cols <- names(test_nif)
  expect_true(all(original_cols %in% names(result)))
  expect_true("BL_CRCL" %in% names(result))
})


test_that("add_bl_crcl calculates correct values for known inputs (egfr_cg)", {
  # Known test case: Male, age 45, weight 70kg, creat 0.8 mg/dl
  # CG formula: (140 - age) * weight * sex_factor / 72 / crea
  # Male: (140 - 45) * 70 * 1 / 72 / 0.8 = 95 * 70 / 72 / 0.8 = 115.45
  test_nif <- tibble::tribble(
    ~ID, ~USUBJID,    ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~BL_CREAT, ~AGE, ~SEX, ~RACE,     ~WEIGHT,
    1,   "SUBJ-001",  0,     0,    1,    0,     NA,  0.8,       45,   0,    "WHITE",  70
  ) %>%
    nif()

  result <- add_bl_crcl(test_nif, method = egfr_cg, molar = FALSE)

  expected_crcl <- (140 - 45) * 70 * 1 / 72 / 0.8
  expect_equal(result$BL_CRCL[1], expected_crcl, tolerance = 0.01)
})


test_that("add_bl_crcl works with molar units (umol/l)", {
  # Test that molar=TRUE is passed to the method
  # Creatinine 70.72 umol/l = 0.8 mg/dl (70.72 / 88.4 = 0.8)
  test_nif <- tibble::tribble(
    ~ID, ~USUBJID,    ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~BL_CREAT, ~AGE, ~SEX, ~RACE,     ~WEIGHT,
    1,   "SUBJ-001",  0,     0,    1,    0,     NA,  70.72,     45,   0,    "WHITE",  70
  ) %>%
    nif()

  result <- add_bl_crcl(test_nif, method = egfr_cg)

  # Should calculate correctly (molar=TRUE converts umol/l to mg/dl)
  expect_true("BL_CRCL" %in% names(result))
  expect_false(is.na(result$BL_CRCL[1]))
  expect_s3_class(result, "nif")
})


test_that("add_bl_crcl handles edge case with very high creatinine", {
  test_nif <- tibble::tribble(
    ~ID, ~USUBJID,    ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~BL_CREAT, ~AGE, ~SEX, ~RACE,     ~WEIGHT,
    1,   "SUBJ-001",  0,     0,    1,    0,     NA,  5.0,       45,   0,    "WHITE",  70
  ) %>%
    nif()

  result <- add_bl_crcl(test_nif, method = egfr_cg)

  # Should still calculate (though value will be low)
  expect_true("BL_CRCL" %in% names(result))
  expect_false(is.na(result$BL_CRCL[1]))
  expect_true(result$BL_CRCL[1] > 0)  # Should be positive
})


test_that("add_bl_crcl handles edge case with very low creatinine", {
  test_nif <- tibble::tribble(
    ~ID, ~USUBJID,    ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~BL_CREAT, ~AGE, ~SEX, ~RACE,     ~WEIGHT,
    1,   "SUBJ-001",  0,     0,    1,    0,     NA,  0.3,       45,   0,    "WHITE",  70
  ) %>%
    nif()

  result <- add_bl_crcl(test_nif, method = egfr_cg)

  # Should still calculate (though value will be high)
  expect_true("BL_CRCL" %in% names(result))
  expect_false(is.na(result$BL_CRCL[1]))
  expect_true(result$BL_CRCL[1] > 0)  # Should be positive
})


test_that("add_bl_crcl handles different age values", {
  test_nif_young <- tibble::tribble(
    ~ID, ~USUBJID,    ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~BL_CREAT, ~AGE, ~SEX, ~RACE,     ~WEIGHT,
    1,   "SUBJ-001",  0,     0,    1,    0,     NA,  0.8,       25,   0,    "WHITE",  70
  ) %>%
    nif()

  result_young <- add_bl_crcl(test_nif_young, method = egfr_cg)

  test_nif_old <- tibble::tribble(
    ~ID, ~USUBJID,    ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~BL_CREAT, ~AGE, ~SEX, ~RACE,     ~WEIGHT,
    1,   "SUBJ-001",  0,     0,    1,    0,     NA,  0.8,       75,   0,    "WHITE",  70
  ) %>%
    nif()

  result_old <- add_bl_crcl(test_nif_old, method = egfr_cg)

  # Younger person should have higher CRCL (all else equal)
  expect_true(result_young$BL_CRCL[1] > result_old$BL_CRCL[1])
})


test_that("add_bl_crcl handles different weight values", {
  test_nif_light <- tibble::tribble(
    ~ID, ~USUBJID,    ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~BL_CREAT, ~AGE, ~SEX, ~RACE,     ~WEIGHT,
    1,   "SUBJ-001",  0,     0,    1,    0,     NA,  0.8,       45,   0,    "WHITE",  50
  ) %>%
    nif()

  result_light <- add_bl_crcl(test_nif_light, method = egfr_cg)

  test_nif_heavy <- tibble::tribble(
    ~ID, ~USUBJID,    ~TIME, ~AMT, ~CMT, ~EVID, ~DV, ~BL_CREAT, ~AGE, ~SEX, ~RACE,     ~WEIGHT,
    1,   "SUBJ-001",  0,     0,    1,    0,     NA,  0.8,       45,   0,    "WHITE",  90
  ) %>%
    nif()

  result_heavy <- add_bl_crcl(test_nif_heavy, method = egfr_cg)

  # Heavier person should have higher CRCL (all else equal, for CG method)
  expect_true(result_heavy$BL_CRCL[1] > result_light$BL_CRCL[1])
})


test_that("add_bl_crcl works with actual examplinib data", {
  # Test with actual package data
  # Check if required columns exist
  required_cols <- c("BL_CREAT", "AGE", "SEX", "RACE", "WEIGHT")
  if (all(required_cols %in% names(examplinib_poc_nif))) {
    result <- add_bl_crcl(examplinib_poc_nif)

    expect_true("BL_CRCL" %in% names(result))
    expect_s3_class(result, "nif")
    expect_equal(nrow(result), nrow(examplinib_poc_nif))

    # Check that values are calculated
    expect_false(all(is.na(result$BL_CRCL)))
  } else {
    skip("Required columns not available in examplinib_poc_nif")
  }
})

