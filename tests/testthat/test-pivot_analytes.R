# Test file for pivot_analytes function

test_that("pivot_analytes input validation", {
  # Test that non-nif objects are rejected
  expect_error(pivot_analytes(data.frame()), "Input must be a nif object")

  # Test that regular data.frame is rejected
  test_df <- data.frame(ID = 1, TIME = 0, DV = 10, EVID = 0, ANALYTE = "A")
  expect_error(pivot_analytes(test_df), "Input must be a nif object")
})


test_that("pivot_analytes requires NTIME field", {
  # Create nif object without NTIME
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~EVID, ~ANALYTE, ~DTC,
    1,     0,   0,     1,   "DRUG",   "2023-01-01T00:00:00",
    1,     0,  10,     0,   "DRUG",   "2023-01-01T00:00:00"
  ) %>% nif()

  expect_error(pivot_analytes(test_nif), "'NTIME' not found in nif object!")
})


test_that("pivot_analytes validates duplicate parameter", {
  # Create valid test nif object
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~EVID, ~ANALYTE, ~DTC, ~NTIME,
    1,     0,   0,     1,   "DRUG",   "2023-01-01T00:00:00", 0,
    1,     0,  10,     0,   "DRUG",   "2023-01-01T00:00:00", 0
  ) %>% nif()

  # Test invalid duplicate parameter
  expect_error(pivot_analytes(test_nif, duplicates = "invalid"),
               "Invalid value for 'duplicates' - must be one of")

  # Test that "ignore" is not implemented (documented but not in validation)
  expect_error(pivot_analytes(test_nif, duplicates = "ignore"),
               "Invalid value for 'duplicates' - must be one of")
})


test_that("pivot_analytes validates keep fields", {
  # Create valid test nif object
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~EVID, ~ANALYTE, ~DTC, ~NTIME,
    1,     0,   0,     1,   "DRUG",   "2023-01-01T00:00:00", 0,
    1,     0,  10,     0,   "DRUG",   "2023-01-01T00:00:00", 0
  ) %>% nif()

  # Test missing keep field
  expect_error(pivot_analytes(test_nif, keep = "NONEXISTENT"),
               "Missing keep field: NONEXISTENT")
})


test_that("pivot_analytes validates keep fields are unique per subject", {
  # Create nif object with non-unique keep fields per subject
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~EVID, ~ANALYTE, ~DTC, ~NTIME, ~AGE,
    1,     0,   0,     1,   "DRUG",   "2023-01-01T00:00:00", 0, 25,
    1,     0,  10,     0,   "DRUG",   "2023-01-01T00:00:00", 0, 30,  # Different AGE for same ID
    2,     0,   0,     1,   "DRUG",   "2023-01-01T00:00:00", 0, 35,
    2,     0,  15,     0,   "DRUG",   "2023-01-01T00:00:00", 0, 35
  ) %>% nif()

  expect_error(pivot_analytes(test_nif, keep = "AGE"),
               "Keep fields \\(AGE\\) are not unique for the following subjects: 1")
})


test_that("pivot_analytes basic functionality works", {
  # Create test nif object with multiple analytes and time points
  test_nif <- tibble::tribble(
    ~ID,   ~TIME, ~DV, ~EVID, ~ANALYTE, ~DTC, ~NTIME, ~AGE, ~SEX,
    1,     0,   0,     1,   "DRUG",   "2023-01-01T00:00:00", 0, 25, "M",
    1,     0,  10,     0,   "DRUG",   "2023-01-01T00:00:00", 0, 25, "M",
    1,     2,  20,     0,   "DRUG",   "2023-01-01T02:00:00", 2, 25, "M",
    1,     0,   5,     0,   "METAB",  "2023-01-01T00:00:00", 0, 25, "M",
    1,     2,  15,     0,   "METAB",  "2023-01-01T02:00:00", 2, 25, "M",
    2,     0,   0,     1,   "DRUG",   "2023-01-01T00:00:00", 0, 30, "F",
    2,     0,  12,     0,   "DRUG",   "2023-01-01T00:00:00", 0, 30, "F",
    2,     2,  22,     0,   "DRUG",   "2023-01-01T02:00:00", 2, 30, "F"
  ) %>%
    nif() %>%
    lubrify_dates()

  result <- pivot_analytes(test_nif, keep = c("AGE", "SEX"))

  # Check structure
  expect_s3_class(result, "data.frame")
  expect_false(inherits(result, "nif"))  # Should return regular data.frame

  # Check that analytes are pivoted to columns
  expect_true("DRUG" %in% names(result))
  expect_true("METAB" %in% names(result))

  # Check that keep fields are preserved
  expect_true("AGE" %in% names(result))
  expect_true("SEX" %in% names(result))

  # Check that ID and time fields are preserved
  expect_true("ID" %in% names(result))
  expect_true("NTIME" %in% names(result))
  expect_true("TRTDY" %in% names(result))

  # Check that ANALYTE and DV are removed
  expect_false("ANALYTE" %in% names(result))
  expect_false("DV" %in% names(result))
})


test_that("pivot_analytes handles single analyte", {
  # Create test nif object with single analyte
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~EVID, ~ANALYTE, ~DTC, ~NTIME, ~AGE,
    1,     0,   0,     1,   "DRUG",   "2023-01-01T00:00:00", 0, 25,
    1,     0,  10,     0,   "DRUG",   "2023-01-01T00:00:00", 0, 25,
    1,     2,  20,     0,   "DRUG",   "2023-01-01T02:00:00", 2, 25,
    2,     0,   0,     1,   "DRUG",   "2023-01-01T00:00:00", 0, 30,
    2,     0,  12,     0,   "DRUG",   "2023-01-01T00:00:00", 0, 30
  ) %>%
    nif() %>%
    lubrify_dates()

  result <- pivot_analytes(test_nif, keep = "AGE")

  # Should have DRUG column
  expect_true("DRUG" %in% names(result))

  # Should have correct number of rows (unique ID/NTIME combinations)
  expect_equal(nrow(result), 3)  # ID=1,NTIME=0; ID=1,NTIME=2; ID=2,NTIME=0
})


test_that("pivot_analytes filters by analyte parameter", {
  # Create test nif object with multiple analytes
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~EVID, ~ANALYTE, ~DTC, ~NTIME, ~AGE,
    1,     0,   0,     1,   "DRUG",   "2023-01-01T00:00:00", 0, 25,
    1,     0,  10,     0,   "DRUG",   "2023-01-01T00:00:00", 0, 25,
    1,     0,   5,     0,   "METAB",  "2023-01-01T00:00:00", 0, 25,
    1,     0,   3,     0,   "OTHER",  "2023-01-01T00:00:00", 0, 25
  ) %>%
    nif() %>%
    lubrify_dates()

  # Test filtering to specific analyte
  result <- pivot_analytes(test_nif, analyte = "DRUG", keep = "AGE")

  # Should only have DRUG column
  expect_true("DRUG" %in% names(result))
  expect_false("METAB" %in% names(result))
  expect_false("OTHER" %in% names(result))
})


test_that("pivot_analytes handles NA NTIME values", {
  # Create test nif object with NA NTIME values
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~EVID, ~ANALYTE, ~DTC, ~NTIME, ~AGE,
    1,     0,   0,     1,   "DRUG",   "2023-01-01T00:00:00", 0, 25,
    1,     0,  10,     0,   "DRUG",   "2023-01-01T00:00:00", 0, 25,
    1,     2,  NA,     0,   "DRUG",   "2023-01-01T02:00:00", NA, 25,  # NA NTIME
    1,     4,  20,     0,   "DRUG",   "2023-01-01T04:00:00", 4, 25
  ) %>%
    nif() %>%
    lubrify_dates()

  # Should exclude NA NTIME observations
  result <- pivot_analytes(test_nif, keep = "AGE", silent = TRUE)

  # Should only have 2 rows (NTIME 0 and 4)
  expect_equal(nrow(result), 2)
  expect_true(all(result$NTIME %in% c(0, 4)))
})


test_that("pivot_analytes handles exclusions", {
  # Create test nif object with exclusions
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~EVID, ~ANALYTE, ~DTC, ~NTIME, ~AGE, ~EXCL, ~EXCL_REASON,
    1,     0,   0,     1,   "DRUG",   "2023-01-01T00:00:00", 0, 25, FALSE, NA,
    1,     0,  10,     0,   "DRUG",   "2023-01-01T00:00:00", 0, 25, FALSE, NA,
    1,     2,  20,     0,   "DRUG",   "2023-01-01T02:00:00", 2, 25, TRUE, "Outlier",
    1,     4,  30,     0,   "DRUG",   "2023-01-01T04:00:00", 4, 25, FALSE, NA
  ) %>%
    nif() %>%
    lubrify_dates()

  result <- pivot_analytes(test_nif, keep = "AGE", silent = TRUE)

  # Should exclude the observation with EXCL = TRUE
  expect_equal(nrow(result), 2)  # Only NTIME 0 and 4
  expect_false(2 %in% result$NTIME)
})


test_that("pivot_analytes handles duplicates with stop option", {
  # Create test nif object with duplicates
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~EVID, ~ANALYTE, ~DTC, ~NTIME, ~AGE,
    1,     0,   0,     1,   "DRUG",   "2023-01-01T00:00:00", 0, 25,
    1,     0,  10,     0,   "DRUG",   "2023-01-01T00:00:00", 0, 25,  # Duplicate
    1,     0,  12,     0,   "DRUG",   "2023-01-01T00:00:00", 0, 25   # Duplicate
  ) %>%
    nif() %>%
    lubrify_dates()

  expect_error(pivot_analytes(test_nif, keep = "AGE"),
               "Duplicate observations found with respect to NTIME and TRTDY")
})


test_that("pivot_analytes handles duplicates with identify option", {
  # Create test nif object with duplicates
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~EVID, ~ANALYTE, ~DTC, ~NTIME, ~AGE,
    1,     0,   0,     1,   "DRUG",   "2023-01-01T00:00:00", 0, 25,
    1,     0,  10,     0,   "DRUG",   "2023-01-01T00:00:00", 0, 25,  # Duplicate
    1,     0,  12,     0,   "DRUG",   "2023-01-01T00:00:00", 0, 25   # Duplicate
  ) %>%
    nif() %>%
    lubrify_dates()

  expect_message(
    result <- pivot_analytes(
      test_nif, duplicates = "identify", keep = "AGE", silent = TRUE),
    "Only duplicate observations returned"
  )

  # Should return only duplicate observations
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)  # Two duplicate observations
  expect_true(all(result$ID == 1))
  expect_true(all(result$NTIME == 0))
})


test_that("pivot_analytes handles duplicates with resolve option", {
  # Create test nif object with duplicates
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~EVID, ~ANALYTE, ~DTC, ~NTIME, ~AGE,
    1,     0,   0,     1,   "DRUG",   "2023-01-01T00:00:00", 0, 25,
    1,     0,  10,     0,   "DRUG",   "2023-01-01T00:00:00", 0, 25,  # Duplicate
    1,     0,  12,     0,   "DRUG",   "2023-01-01T00:00:00", 0, 25,  # Duplicate
    1,     2,  20,     0,   "DRUG",   "2023-01-01T02:00:00", 2, 25
  ) %>%
    nif() %>%
    lubrify_dates()

  result <- pivot_analytes(test_nif, duplicates = "resolve", keep = "AGE",
                           silent = TRUE)

  # Should resolve duplicates (mean of 10 and 12 = 11)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)  # ID=1, NTIME=0; ID=1, NTIME=2

  # Check that duplicate was resolved to mean
  drug_values <- result$DRUG[result$NTIME == 0]
  expect_equal(drug_values, 11)  # Mean of 10 and 12
})


test_that("pivot_analytes handles custom duplicate function", {
  # Create test nif object with duplicates
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~EVID, ~ANALYTE, ~DTC, ~NTIME, ~AGE,
    1,     0,   0,     1,   "DRUG",   "2023-01-01T00:00:00", 0, 25,
    1,     0,  10,     0,   "DRUG",   "2023-01-01T00:00:00", 0, 25,  # Duplicate
    1,     0,  12,     0,   "DRUG",   "2023-01-01T00:00:00", 0, 25   # Duplicate
  ) %>%
    nif() %>%
    lubrify_dates()

  # Test with sum function
  result <- pivot_analytes(test_nif, duplicates = "resolve",
                          duplicate_function = sum, keep = "AGE", silent = TRUE)

  # Should resolve duplicates using sum (10 + 12 = 22)
  drug_values <- result$DRUG[result$NTIME == 0]
  expect_equal(drug_values, 22)
})


test_that("pivot_analytes handles empty data after filtering", {
  # Create test nif object with all NA NTIME
  test_nif <- tibble::tribble(
    ~ID,   ~TIME, ~DV, ~EVID, ~ANALYTE, ~DTC,                ~NTIME, ~AGE,
    1,     0,   0,     1,   "DRUG",   "2023-01-01T00:00:00", 0, 25,
    1,     0,  NA,     0,   "DRUG",   "2023-01-01T00:00:00", NA, 25,
    1,     2,  NA,     0,   "DRUG",   "2023-01-01T02:00:00", NA, 25
  ) %>%
    nif() %>%
    lubrify_dates()

  expect_message(
    result <- pivot_analytes(test_nif, keep = "AGE", silent = TRUE),
    "NTIME is all NA"
  )

  # Should return empty data frame with correct structure
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})


test_that("pivot_analytes uses default keep fields when NULL", {
  # Create test nif object with some standard fields
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~EVID, ~ANALYTE, ~DTC, ~NTIME, ~AGE, ~SEX, ~STUDYID,
    1,     0,   0,     1,   "DRUG",   "2023-01-01T00:00:00", 0, 25, "M", "STUDY001",
    1,     0,  10,     0,   "DRUG",   "2023-01-01T00:00:00", 0, 25, "M", "STUDY001"
  ) %>%
    nif() %>%
    lubrify_dates()

  result <- pivot_analytes(test_nif)

  # Should include standard fields that exist in the data
  expect_true("AGE" %in% names(result))
  expect_true("SEX" %in% names(result))
  expect_true("STUDYID" %in% names(result))
})


# test_that("pivot_analytes works with real examplinib data", {
#   # Test with actual examplinib data if available
#   if (exists("examplinib_poc_min_nif")) {
#     # This test will only run if the data is available
#     result <- pivot_analytes(examplinib_poc_min_nif)
#
#     expect_s3_class(result, "data.frame")
#     expect_false(inherits(result, "nif"))
#
#     # Should have analyte columns
#     expect_gt(ncol(result), 5)  # Should have more than just ID, NTIME, TRTDY, etc.
#   }
# })


test_that("pivot_analytes handles multiple subjects correctly", {
  # Create test nif object with multiple subjects
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~EVID, ~ANALYTE, ~DTC, ~NTIME, ~AGE,
    1,     0,   0,     1,   "DRUG",   "2023-01-01T00:00:00", 0, 25,
    1,     0,  10,     0,   "DRUG",   "2023-01-01T00:00:00", 0, 25,
    1,     2,  20,     0,   "DRUG",   "2023-01-01T02:00:00", 2, 25,
    2,     0,   0,     1,   "DRUG",   "2023-01-01T00:00:00", 0, 30,
    2,     0,  15,     0,   "DRUG",   "2023-01-01T00:00:00", 0, 30,
    2,     2,  25,     0,   "DRUG",   "2023-01-01T02:00:00", 2, 30
  ) %>%
    nif() %>%
    lubrify_dates()

  result <- pivot_analytes(test_nif, keep = "AGE")

  # Should have 4 rows (2 subjects × 2 time points each)
  expect_equal(nrow(result), 4)

  # Should have both subjects
  expect_equal(sort(unique(result$ID)), c(1, 2))

  # Should have both time points
  expect_equal(sort(unique(result$NTIME)), c(0, 2))
})


test_that("pivot_analytes preserves TRTDY calculation", {
  # Create test nif object to test TRTDY calculation
  test_nif <- tibble::tribble(
    ~ID, ~TIME, ~DV, ~EVID, ~ANALYTE, ~DTC, ~NTIME, ~AGE,
    1,     0,   0,     1,   "DRUG",   "2023-01-01T00:00:00", 0, 25,
    1,     0,  10,     0,   "DRUG",   "2023-01-01T00:00:00", 0, 25,
    1,     2,  20,     0,   "DRUG",   "2023-01-01T02:00:00", 2, 25
  ) %>%
    nif() %>%
    lubrify_dates()

  result <- pivot_analytes(test_nif, keep = "AGE")

  # Should have TRTDY column
  expect_true("TRTDY" %in% names(result))

  # TRTDY should be calculated correctly (add_trtdy function should handle this)
  expect_true(all(!is.na(result$TRTDY)))
})
