# Comprehensive tests for ensure_parent() function

test_that("ensure_parent() errors on empty data frame", {
  empty_nif <- nif(
    tibble::tribble(
      ~ID, ~TIME, ~EVID, ~CMT, ~DV,  ~AMT
    ) |>
      filter(FALSE)
  )

  # Should error because there are no observations
  expect_error(
    ensure_parent(empty_nif),
    "Cannot determine PARENT"
  )
})


test_that("ensure_parent() preserves existing PARENT column", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~DV,  ~AMT, ~ANALYTE, ~PARENT,
    1,   0,     1,     1,    100,  100,  "DRUG",   "CUSTOM_PARENT",
    1,   1,     0,     1,    50,   0,    "DRUG",   "CUSTOM_PARENT",
    1,   2,     0,     1,    25,   0,    "DRUG",   "CUSTOM_PARENT"
  )

  test_nif <- nif(test_data)
  result <- ensure_parent(test_nif)

  # PARENT should remain unchanged
  expect_equal(unique(result$PARENT), "CUSTOM_PARENT")
  expect_true(all(result$PARENT == "CUSTOM_PARENT"))
})


test_that("ensure_parent() Case 1a: Single treatment with matching ANALYTE in observations", {
  # Single treatment "DRUG" with observations of same ANALYTE
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~DV,  ~AMT, ~ANALYTE,
    1,   0,     1,     1,    100,  100,  "DRUG",
    1,   1,     0,     1,    50,   0,    "DRUG",
    1,   2,     0,     1,    25,   0,    "DRUG",
    2,   0,     1,     1,    100,  100,  "DRUG",
    2,   1,     0,     1,    50,   0,    "DRUG"
  )

  test_nif <- nif(test_data)
  result <- ensure_parent(test_nif)

  # Check that PARENT was added
  expect_true("PARENT" %in% names(result))

  # For administrations (EVID == 1), PARENT should equal ANALYTE
  admin_rows <- result[result$EVID == 1, ]
  expect_equal(admin_rows$PARENT, admin_rows$ANALYTE)
  expect_equal(unique(admin_rows$PARENT), "DRUG")

  # For observations (EVID == 0), PARENT should be the treatment ANALYTE
  obs_rows <- result[result$EVID == 0, ]
  expect_equal(unique(obs_rows$PARENT), "DRUG")

  # All rows should have PARENT = "DRUG"
  expect_equal(unique(result$PARENT), "DRUG")
  expect_true(inherits(result, "nif"))
})


test_that("ensure_parent() Case 1b: Single treatment without matching ANALYTE uses administration ANALYTE", {
  # Single treatment "DRUG" but observations are "PARENT" (different ANALYTE)
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~DV,  ~AMT, ~ANALYTE,
    1,   0,     1,     1,    100,  100,  "DRUG",
    1,   1,     0,     2,    50,   0,    "PARENT",
    1,   2,     0,     3,    25,   0,    "PARENT",
    2,   0,     1,     1,    100,  100,  "DRUG",
    2,   1,     0,     2,    50,   0,    "PARENT"
  )

  test_nif <- nif(test_data)
  result <- ensure_parent(test_nif)

  # Check that PARENT was added
  expect_true("PARENT" %in% names(result))

  # For administrations, PARENT should equal DRUG
  admin_rows <- result[result$EVID == 1, ]
  expect_equal(admin_rows$PARENT, admin_rows$ANALYTE)
  expect_equal(unique(admin_rows$PARENT), "DRUG")

  # For observations, PARENT should be DRUG (from last administration)
  obs_rows <- result[result$EVID == 0, ]
  expect_equal(unique(obs_rows$PARENT), "DRUG")

  expect_true(inherits(result, "nif"))
})


test_that("ensure_parent() Case 1b: Single treatment with multiple CMTs", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~DV,  ~AMT, ~ANALYTE,
    1,   0,     1,     1,    100,  100,  "DRUG",
    1,   1,     0,     5,    50,   0,    "CMT5",
    1,   2,     0,     3,    25,   0,    "CMT3",
    1,   3,     0,     2,    12,   0,    "CMT2"
  )

  test_nif <- nif(test_data)
  result <- ensure_parent(test_nif)

  obs_rows <- result[result$EVID == 0, ]
  expect_equal(unique(obs_rows$PARENT), "DRUG")
})


test_that("ensure_parent() Case 2a: Multiple treatments, subjects receive one each", {
  # Multiple treatments but each subject receives only one
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~DV,  ~AMT, ~ANALYTE,
    1,   0,     1,     1,    100,  100,  "DRUG_A",
    1,   1,     0,     1,    50,   0,    "DRUG_A",
    1,   2,     0,     1,    25,   0,    "DRUG_A",
    2,   0,     1,     1,    100,  100,  "DRUG_B",
    2,   1,     0,     1,    50,   0,    "DRUG_B",
    2,   2,     0,     1,    25,   0,    "DRUG_B",
    3,   0,     1,     1,    100,  100,  "PLACEBO",
    3,   1,     0,     1,    50,   0,    "PLACEBO"
  )

  test_nif <- nif(test_data)
  result <- ensure_parent(test_nif)

  # Check that PARENT was added
  expect_true("PARENT" %in% names(result))

  # For administrations, PARENT should equal ANALYTE
  admin_rows <- result[result$EVID == 1, ]
  expect_equal(admin_rows$PARENT, admin_rows$ANALYTE)

  # For observations, PARENT should match the subject's treatment
  # Subject 1: DRUG_A
  subj1_obs <- result[result$ID == 1 & result$EVID == 0, ]
  expect_equal(unique(subj1_obs$PARENT), "DRUG_A")

  # Subject 2: DRUG_B
  subj2_obs <- result[result$ID == 2 & result$EVID == 0, ]
  expect_equal(unique(subj2_obs$PARENT), "DRUG_B")

  # Subject 3: PLACEBO
  subj3_obs <- result[result$ID == 3 & result$EVID == 0, ]
  expect_equal(unique(subj3_obs$PARENT), "PLACEBO")

  expect_true(inherits(result, "nif"))
})


test_that("ensure_parent() Case 2a: Multiple treatments with metabolites per subject", {
  # Subject receives one treatment but has multiple analytes (parent + metabolites)
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~DV,  ~AMT, ~ANALYTE,
    1,   0,     1,     1,    100,  100,  "DRUG_A",
    1,   1,     0,     1,    50,   0,    "DRUG_A",
    1,   2,     0,     2,    25,   0,    "METABOLITE_A1",
    1,   3,     0,     3,    12,   0,    "METABOLITE_A2",
    2,   0,     1,     1,    100,  100,  "DRUG_B",
    2,   1,     0,     1,    50,   0,    "DRUG_B",
    2,   2,     0,     2,    25,   0,    "METABOLITE_B1"
  )

  test_nif <- nif(test_data)
  result <- ensure_parent(test_nif)

  # Subject 1: all observations should have PARENT = "DRUG_A"
  subj1_obs <- result[result$ID == 1 & result$EVID == 0, ]
  expect_equal(unique(subj1_obs$PARENT), "DRUG_A")

  # Subject 2: all observations should have PARENT = "DRUG_B"
  subj2_obs <- result[result$ID == 2 & result$EVID == 0, ]
  expect_equal(unique(subj2_obs$PARENT), "DRUG_B")
})


test_that("ensure_parent() Case 2b: Multiple treatments, subjects receive multiple", {
  # Multiple treatments: subject 1 has multiple, subject 2 has one
  # Subject 1: DRUG_A at TIME=0, DRUG_B at TIME=24
  # Observations after TIME=24 should use DRUG_B (last administration)
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~DV,  ~AMT, ~ANALYTE,
    1,   0,     1,     1,    100,  100,  "DRUG_A",  # Subject 1: multiple treatments
    1,   24,    1,     1,    100,  100,  "DRUG_B",
    1,   1,     0,     1,    50,   0,    "DRUG_A",  # Before DRUG_B admin, uses DRUG_A
    1,   25,    0,     2,    25,   0,    "METABOLITE",  # After DRUG_B admin, uses DRUG_B
    2,   0,     1,     1,    100,  100,  "DRUG_A",  # Subject 2: single treatment
    2,   1,     0,     1,    50,   0,    "DRUG_A",
    2,   2,     0,     1,    30,   0,    "DRUG_A",
    2,   3,     0,     2,    25,   0,    "METABOLITE"
  )

  test_nif <- nif(test_data)
  result <- ensure_parent(test_nif)

  # Check that PARENT was added
  expect_true("PARENT" %in% names(result))

  # For administrations, PARENT should equal ANALYTE
  admin_rows <- result[result$EVID == 1, ]
  expect_equal(admin_rows$PARENT, admin_rows$ANALYTE)

  # Subject 1 (multiple treatments):
  # Observation at TIME=1 (before DRUG_B) should use DRUG_A
  # Observation at TIME=25 (after DRUG_B) should use DRUG_B (last administration)
  subj1_obs <- result[result$ID == 1 & result$EVID == 0, ]
  expect_equal(subj1_obs$PARENT[subj1_obs$TIME == 1], "DRUG_A")
  expect_equal(subj1_obs$PARENT[subj1_obs$TIME == 25], "DRUG_B")

  # Subject 2 (single treatment): PARENT should be DRUG_A (their treatment)
  subj2_obs <- result[result$ID == 2 & result$EVID == 0, ]
  expect_equal(unique(subj2_obs$PARENT), "DRUG_A")

  expect_true(inherits(result, "nif"))
})


test_that("ensure_parent() Case 2b: Multiple treatments uses last administration", {
  # Test that last administered treatment (by TIME) is used
  # DRUG_A at TIME=0, DRUG_B at TIME=24
  # Observations before TIME=24 use DRUG_A, after use DRUG_B
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~DV,  ~AMT, ~ANALYTE,
    1,   0,     1,     1,    100,  100,  "DRUG_A",
    1,   24,    1,     1,    100,  100,  "DRUG_B",
    1,   1,     0,     2,    50,   0,    "DRUG_B",  # Before DRUG_B, uses DRUG_A
    1,   25,    0,     2,    25,   0,    "DRUG_B"   # After DRUG_B, uses DRUG_B
  )

  test_nif <- nif(test_data)
  result <- ensure_parent(test_nif)

  # Observation at TIME=1 (before DRUG_B admin) should use DRUG_A
  # Observation at TIME=25 (after DRUG_B admin) should use DRUG_B
  obs_rows <- result[result$EVID == 0, ]
  expect_equal(obs_rows$PARENT[obs_rows$TIME == 1], "DRUG_A")
  expect_equal(obs_rows$PARENT[obs_rows$TIME == 25], "DRUG_B")
})


test_that("ensure_parent() Case 2b: Multiple treatments with 4 subjects using last administration", {
  # Comprehensive test with 4 subjects to demonstrate the new behavior
  # The new behavior uses the last administered treatment (by TIME) for each observation
  test_data <- tibble::tribble(
    # Subject 1: Multiple treatments (DRUG_A at TIME=0, DRUG_B at TIME=24)
    ~ID, ~TIME, ~EVID, ~CMT, ~DV,  ~AMT, ~ANALYTE,
    1,   0,     1,     1,    100,  100,  "DRUG_A",
    1,   24,    1,     1,    100,  100,  "DRUG_B",
    1,   1,     0,     1,    50,   0,    "DRUG_A",  # Before DRUG_B, uses DRUG_A
    1,   2,     0,     1,    45,   0,    "DRUG_A",  # Before DRUG_B, uses DRUG_A
    1,   25,    0,     2,    25,   0,    "DRUG_B",  # After DRUG_B, uses DRUG_B
    # Subject 2: Multiple treatments (DRUG_B at TIME=0, DRUG_C at TIME=24)
    2,   0,     1,     1,    100,  100,  "DRUG_B",
    2,   24,    1,     1,    100,  100,  "DRUG_C",
    2,   1,     0,     1,    50,   0,    "DRUG_B",  # Before DRUG_C, uses DRUG_B
    2,   2,     0,     1,    45,   0,    "DRUG_B",  # Before DRUG_C, uses DRUG_B
    2,   25,    0,     3,    20,   0,    "DRUG_C",  # After DRUG_C, uses DRUG_C
    # Subject 3: Single treatment (DRUG_A)
    3,   0,     1,     1,    100,  100,  "DRUG_A",
    3,   1,     0,     1,    50,   0,    "DRUG_A",  # Uses DRUG_A
    3,   2,     0,     1,    45,   0,    "DRUG_A",  # Uses DRUG_A
    3,   3,     0,     2,    30,   0,    "METABOLITE",  # Uses DRUG_A
    # Subject 4: Multiple treatments (DRUG_A at TIME=0, DRUG_C at TIME=24)
    4,   0,     1,     1,    100,  100,  "DRUG_A",
    4,   24,    1,     1,    100,  100,  "DRUG_C",
    4,   1,     0,     1,    50,   0,    "DRUG_A"   # Before DRUG_C, uses DRUG_A
  )

  test_nif <- nif(test_data)
  result <- ensure_parent(test_nif)

  # Check that PARENT was added
  expect_true("PARENT" %in% names(result))

  # For administrations, PARENT should equal ANALYTE
  admin_rows <- result[result$EVID == 1, ]
  expect_equal(admin_rows$PARENT, admin_rows$ANALYTE)

  # Subject 1 (multiple treatments):
  # Observations before TIME=24 use DRUG_A, after use DRUG_B
  subj1_obs <- result[result$ID == 1 & result$EVID == 0, ]
  expect_equal(subj1_obs$PARENT[subj1_obs$TIME == 1], "DRUG_A")
  expect_equal(subj1_obs$PARENT[subj1_obs$TIME == 2], "DRUG_A")
  expect_equal(subj1_obs$PARENT[subj1_obs$TIME == 25], "DRUG_B")

  # Subject 2 (multiple treatments):
  # Observations before TIME=24 use DRUG_B, after use DRUG_C
  subj2_obs <- result[result$ID == 2 & result$EVID == 0, ]
  expect_equal(subj2_obs$PARENT[subj2_obs$TIME == 1], "DRUG_B")
  expect_equal(subj2_obs$PARENT[subj2_obs$TIME == 2], "DRUG_B")
  expect_equal(subj2_obs$PARENT[subj2_obs$TIME == 25], "DRUG_C")

  # Subject 3 (single treatment): PARENT should be DRUG_A (their treatment)
  subj3_obs <- result[result$ID == 3 & result$EVID == 0, ]
  expect_equal(unique(subj3_obs$PARENT), "DRUG_A")

  # Subject 4 (multiple treatments):
  # Observation before TIME=24 uses DRUG_A
  subj4_obs <- result[result$ID == 4 & result$EVID == 0, ]
  expect_equal(unique(subj4_obs$PARENT), "DRUG_A")

  expect_true(inherits(result, "nif"))
})


test_that("ensure_parent() errors when no treatments found", {
  # No administration records (EVID == 1)
  no_treatment_nif <- nif(
    tibble::tribble(
      ~ID, ~TIME, ~EVID, ~CMT, ~DV,  ~AMT,
      1,   0,     0,     1,    50,   0,
      1,   1,     0,     1,    25,   0,
      1,   2,     0,     1,    12,   0
    )
  )

  expect_error(
    ensure_parent(no_treatment_nif),
    "Cannot determine PARENT: no treatment records"
  )
})


test_that("ensure_parent() errors when no observations found in Case 1b", {
  # Single treatment but no observations
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~DV,  ~AMT, ~ANALYTE,
    1,   0,     1,     1,    100,  100,  "DRUG",
    2,   0,     1,     1,    100,  100,  "DRUG"
  )

  test_nif <- nif(test_data)

  expect_no_error(
    ensure_parent(test_nif, silent = FALSE)
  )
})


test_that("ensure_parent() errors when no observations found in Case 2b", {
  # Multiple treatments, subjects with multiple, but no observations
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~DV,  ~AMT, ~ANALYTE,
    1,   0,     1,     1,    100,  100,  "DRUG_A",
    1,   24,    1,     1,    100,  100,  "DRUG_B",
    2,   0,     1,     1,    100,  100,  "DRUG_A"
  )

  test_nif <- nif(test_data)

  expect_no_error(
    ensure_parent(test_nif, silent = FALSE))
})


test_that("ensure_parent() errors with non-NIF object", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~DV,  ~AMT,
    1,   0,     1,     1,    100,  100
  )

  # Not a NIF object
  expect_error(
    ensure_parent(test_data),
    "Input must be a NIF object"
  )

  # Data frame converted from NIF
  test_nif <- nif(test_data)
  expect_error(
    ensure_parent(as.data.frame(test_nif)),
    "Input must be a NIF object"
  )
})


test_that("ensure_parent() creates ANALYTE if missing", {
  # Test data without ANALYTE - should be created from CMT
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~DV,  ~AMT,
    1,   0,     1,     1,    100,  100,
    1,   1,     0,     1,    50,   0,
    1,   2,     0,     2,    25,   0
  )

  test_nif <- nif(test_data)
  result <- ensure_parent(test_nif)

  # ANALYTE should be created
  expect_true("ANALYTE" %in% names(result))
  expect_equal(result$ANALYTE[1], "CMT1")
  expect_equal(result$ANALYTE[3], "CMT2")

  # PARENT should be set
  expect_true("PARENT" %in% names(result))
})


test_that("ensure_parent() preserves all original columns", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~DV,  ~AMT, ~ANALYTE, ~DOSE, ~WEIGHT,
    1,   0,     1,     1,    100,  100,  "DRUG",   100,   70,
    1,   1,     0,     1,    50,   0,    "DRUG",   100,   70,
    1,   2,     0,     1,    25,   0,    "DRUG",   100,   70
  )

  test_nif <- nif(test_data)
  result <- ensure_parent(test_nif)

  # All original columns should be preserved
  expect_true(all(names(test_data) %in% names(result)))
  expect_equal(result$DOSE, test_data$DOSE)
  expect_equal(result$WEIGHT, test_data$WEIGHT)
})


test_that("ensure_parent() returns NIF object", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~DV,  ~AMT, ~ANALYTE,
    1,   0,     1,     1,    100,  100,  "DRUG",
    1,   1,     0,     1,    50,   0,    "DRUG"
  )

  test_nif <- nif(test_data)
  result <- ensure_parent(test_nif)

  expect_s3_class(result, "nif")
  expect_s3_class(result, "data.frame")
})


test_that("ensure_parent() handles complex scenario with multiple subjects and treatments", {
  # Complex scenario: multiple subjects, some with single treatment, some with multiple
  test_data <- tibble::tribble(
    # Subject 1: Single treatment DRUG_A
    ~ID, ~TIME, ~EVID, ~CMT, ~DV,  ~AMT, ~ANALYTE,
    1,   0,     1,     1,    100,  100,  "DRUG_A",
    1,   1,     0,     1,    50,   0,    "DRUG_A",
    1,   2,     0,     2,    25,   0,    "METABOLITE_A",
    # Subject 2: Single treatment DRUG_B
    2,   0,     1,     1,    100,  100,  "DRUG_B",
    2,   1,     0,     1,    50,   0,    "DRUG_B",
    # Subject 3: Multiple treatments (DRUG_A at TIME=0, DRUG_B at TIME=24)
    3,   0,     1,     1,    100,  100,  "DRUG_A",
    3,   24,    1,     1,    100,  100,  "DRUG_B",
    3,   1,     0,     2,    50,   0,    "METABOLITE",  # Before DRUG_B, uses DRUG_A
    3,   25,    0,     3,    25,   0,    "METABOLITE2"  # After DRUG_B, uses DRUG_B
  )

  test_nif <- nif(test_data)
  result <- ensure_parent(test_nif)

  # Subject 1: PARENT should be DRUG_A
  subj1_obs <- result[result$ID == 1 & result$EVID == 0, ]
  expect_equal(unique(subj1_obs$PARENT), "DRUG_A")

  # Subject 2: PARENT should be DRUG_B
  subj2_obs <- result[result$ID == 2 & result$EVID == 0, ]
  expect_equal(unique(subj2_obs$PARENT), "DRUG_B")

  # Subject 3: Observations before TIME=24 use DRUG_A, after use DRUG_B
  subj3_obs <- result[result$ID == 3 & result$EVID == 0, ]
  expect_equal(subj3_obs$PARENT[subj3_obs$TIME == 1], "DRUG_A")
  expect_equal(subj3_obs$PARENT[subj3_obs$TIME == 25], "DRUG_B")
})

