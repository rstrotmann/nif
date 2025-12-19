test_that("dose_red_sbs works with real data", {
  result <- dose_red_sbs(examplinib_poc_nif)
  expect_true(length(result) > 0)
  expect_s3_class(result, "data.frame")
  expect_true(all(c("ID") %in% names(result)))
})


test_that("dose_red_sbs identifies subjects with dose reductions", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~EVID, ~CMT, ~DV,
    1, 0, 100, 1, 1, NA, # Initial dose 100
    1, 24, 80, 1, 1, NA, # Reduced to 80
    1, 48, 60, 1, 1, NA, # Reduced to 60
    1, 1, 0, 0, 2, 5, # Observation

    2, 0, 100, 1, 1, NA, # No reduction
    2, 24, 100, 1, 1, NA, # Same dose
    2, 1, 0, 0, 2, 10 # Observation
  ) %>%
    new_nif()

  result <- dose_red_sbs(nif)

  expect_equal(nrow(result), 1)
  expect_equal(result$ID, 1)
  expect_true(2 %in% nif$ID[nif$EVID == 1]) # Subject 2 has doses but no reduction
  expect_false(2 %in% result$ID) # Subject 2 should not be in results
})


test_that("dose_red_sbs returns USUBJID when available", {
  nif <- tibble::tribble(
    ~ID, ~USUBJID, ~TIME, ~AMT, ~EVID, ~CMT, ~DV,
    1, "SUBJ001", 0, 100, 1, 1, NA,
    1, "SUBJ001", 24, 80, 1, 1, NA,
    2, "SUBJ002", 0, 100, 1, 1, NA,
    2, "SUBJ002", 24, 90, 1, 1, NA
  ) %>%
    new_nif()

  result <- dose_red_sbs(nif)

  expect_equal(nrow(result), 2)
  expect_true("USUBJID" %in% names(result))
  expect_equal(sort(result$USUBJID), c("SUBJ001", "SUBJ002"))
  expect_equal(result$ID[result$USUBJID == "SUBJ001"], 1)
  expect_equal(result$ID[result$USUBJID == "SUBJ002"], 2)
})


test_that("dose_red_sbs works without USUBJID", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~EVID, ~CMT, ~DV,
    1, 0, 100, 1, 1, NA,
    1, 24, 80, 1, 1, NA
  ) %>%
    new_nif()

  result <- dose_red_sbs(nif)

  expect_equal(nrow(result), 1)
  expect_equal(result$ID, 1)
  # The function should work whether or not USUBJID is in the result
  # The important thing is that it correctly identifies the subject with dose reduction
})


test_that("dose_red_sbs handles no dose reductions", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~EVID, ~CMT, ~DV,
    1, 0, 100, 1, 1, NA,
    1, 24, 100, 1, 1, NA, # Same dose
    1, 48, 100, 1, 1, NA, # Same dose
    2, 0, 50, 1, 1, NA,
    2, 24, 50, 1, 1, NA # Same dose
  ) %>%
    new_nif()

  result <- dose_red_sbs(nif)

  expect_equal(nrow(result), 0)
  expect_true("ID" %in% names(result))
})


test_that("dose_red_sbs excludes zero doses from reduction detection", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~EVID, ~CMT, ~DV,
    1, 0, 100, 1, 1, NA, # Initial dose 100
    1, 24, 0, 1, 1, NA, # Zero dose (should be excluded)
    1, 48, 50, 1, 1, NA # Reduced to 50 (should be detected)
  ) %>%
    new_nif()

  result <- dose_red_sbs(nif)

  expect_equal(nrow(result), 1)
  expect_equal(result$ID, 1)
  # Should detect the reduction from 100 to 50, ignoring the zero dose
})


test_that("dose_red_sbs handles subjects with dose increases", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~EVID, ~CMT, ~DV,
    1, 0, 100, 1, 1, NA, # Initial dose 100
    1, 24, 150, 1, 1, NA, # Increased (should not be detected)
    2, 0, 100, 1, 1, NA, # Initial dose 100
    2, 24, 80, 1, 1, NA # Reduced (should be detected)
  ) %>%
    new_nif()

  result <- dose_red_sbs(nif)

  expect_equal(nrow(result), 1)
  expect_equal(result$ID, 2)
  expect_false(1 %in% result$ID)
})


test_that("dose_red_sbs works with ANALYTE column", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~EVID, ~CMT, ~ANALYTE, ~DV,
    1, 0, 100, 1, 1, "DRUG1", NA,
    1, 24, 80, 1, 1, "DRUG1", NA
  ) %>%
    new_nif()

  result <- dose_red_sbs(nif)

  expect_equal(nrow(result), 1)
  expect_equal(result$ID, 1)
})


test_that("dose_red_sbs creates ANALYTE from CMT when missing", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~EVID, ~CMT, ~DV,
    1, 0, 100, 1, 1, NA,
    1, 24, 80, 1, 1, NA,
    2, 0, 100, 1, 1, NA,
    2, 24, 100, 1, 1, NA
  ) %>%
    new_nif()

  # Should not error even without ANALYTE column
  result <- dose_red_sbs(nif)

  expect_equal(nrow(result), 1)
  expect_equal(result$ID, 1)
})


test_that("dose_red_sbs filters by analyte parameter", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~EVID, ~CMT, ~ANALYTE, ~DV,
    1, 0, 100, 1, 1, "DRUG1", NA,
    1, 24, 80, 1, 1, "DRUG1", NA,
    2, 0, 200, 1, 2, "DRUG2", NA,
    2, 24, 200, 1, 2, "DRUG2", NA # No reduction
  ) %>%
    new_nif()

  result <- dose_red_sbs(nif, analyte = "DRUG1")

  expect_equal(nrow(result), 1)
  expect_equal(result$ID, 1)

  # Should only detect reduction in DRUG1, not DRUG2
})


test_that("dose_red_sbs errors on multiple treatments without analyte specification", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~EVID, ~CMT, ~ANALYTE, ~DV,
    1, 0, 100, 1, 1, "DRUG1", NA,
    1, 24, 80, 1, 1, "DRUG1", NA,
    1, 0, 200, 1, 2, "DRUG2", NA,
    1, 24, 150, 1, 2, "DRUG2", NA
  ) %>%
    new_nif()

  expect_error(
    dose_red_sbs(nif),
    "Multiple treatments"
  )
})


# test_that("dose_red_sbs handles multiple compartments per subject", {
#   nif <- tibble::tribble(
#     ~ID, ~TIME, ~AMT, ~EVID, ~CMT, ~ANALYTE, ~DV,
#     1,   0,    100,  1,     1,    "DRUG1",  NA,  # CMT 1, initial dose 100
#     1,   24,   80,   1,     1,    "DRUG1",  NA,  # CMT 1, reduced to 80
#     1,   0,    50,   1,     2,    "DRUG1",  NA,   # CMT 2, initial dose 50 (same analyte)
#     1,   24,   50,   1,     2,    "DRUG1",  NA,   # CMT 2, same dose (same analyte)
#     1,   1,    0,    0,     3,    "DRUG1",  5     # Observation
#   ) %>%
#     new_nif()
#
#   result <- dose_red_sbs(nif)
#
#   # Should detect reduction in CMT 1
#   expect_equal(nrow(result), 1)
#   expect_equal(result$ID, 1)
# })


test_that("dose_red_sbs handles sequential dose reductions correctly", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~EVID, ~CMT, ~DV,
    1, 0, 100, 1, 1, NA, # Initial dose 100
    1, 24, 80, 1, 1, NA, # First reduction to 80
    1, 48, 60, 1, 1, NA, # Second reduction to 60
    1, 72, 60, 1, 1, NA # Same as previous
  ) %>%
    new_nif()

  result <- dose_red_sbs(nif)

  # Should identify subject with any reduction
  expect_equal(nrow(result), 1)
  expect_equal(result$ID, 1)
})


test_that("dose_red_sbs returns distinct subjects", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~EVID, ~CMT, ~DV,
    1, 0, 100, 1, 1, NA,
    1, 24, 80, 1, 1, NA, # Reduction
    1, 48, 60, 1, 1, NA, # Another reduction (same subject)
    2, 0, 100, 1, 1, NA,
    2, 24, 90, 1, 1, NA # Reduction (different subject)
  ) %>%
    new_nif()

  result <- dose_red_sbs(nif)

  # Should return each subject only once
  expect_equal(nrow(result), 2)
  expect_equal(sort(result$ID), c(1, 2))
})


test_that("dose_red_sbs validates NIF object", {
  non_nif <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~EVID, ~CMT,
    1, 0, 100, 1, 1
  )

  expect_error(
    dose_red_sbs(non_nif),
    "Input must be a nif object"
  )
})


test_that("dose_red_sbs validates analyte parameter", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~EVID, ~CMT, ~DV,
    1, 0, 100, 1, 1, NA,
    1, 24, 80, 1, 1, NA
  ) %>%
    new_nif()

  # Should handle NULL analyte
  expect_no_error(dose_red_sbs(nif, analyte = NULL))

  # Should handle character analyte
  expect_no_error(dose_red_sbs(nif, analyte = "DRUG1"))
})


test_that("dose_red_sbs handles empty NIF object", {
  empty_nif <- new_nif()

  result <- dose_red_sbs(empty_nif)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_true("ID" %in% names(result))
})


test_that("dose_red_sbs handles subjects with only single dose", {
  nif <- tibble::tribble(
    ~ID, ~TIME, ~AMT, ~EVID, ~CMT, ~DV,
    1, 0, 100, 1, 1, NA, # Single dose only
    2, 0, 100, 1, 1, NA,
    2, 24, 80, 1, 1, NA # Has reduction
  ) %>%
    new_nif()

  result <- dose_red_sbs(nif)

  # Should only return subject 2 with reduction
  expect_equal(nrow(result), 1)
  expect_equal(result$ID, 2)
  expect_false(1 %in% result$ID)
})
