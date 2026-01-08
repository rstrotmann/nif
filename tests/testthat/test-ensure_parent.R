# Comprehensive tests for ensure_parent() function

test_that("ensure_parent() handles empty data frame", {
  empty_nif <- nif(
    tibble::tribble(
      ~ID, ~TIME, ~EVID, ~CMT, ~DV,  ~AMT
    ) |>
      filter(FALSE)
  )

  result <- ensure_parent(empty_nif)

  # Should return empty data frame with PARENT column
  expect_true("PARENT" %in% names(result))
  expect_equal(nrow(result), 0)
  expect_true(inherits(result, "nif"))
  expect_type(result$PARENT, "character")
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


test_that("ensure_parent() Case 1b: Single treatment without matching ANALYTE uses lowest CMT", {
  # Single treatment "DRUG" but observations are "METABOLITE" (different ANALYTE)
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~DV,  ~AMT, ~ANALYTE,
    1,   0,     1,     1,    100,  100,  "DRUG",
    1,   1,     0,     2,    50,   0,    "METABOLITE",
    1,   2,     0,     3,    25,   0,    "METABOLITE2",
    2,   0,     1,     1,    100,  100,  "DRUG",
    2,   1,     0,     2,    50,   0,    "METABOLITE"
  )

  test_nif <- nif(test_data)

  # Should issue a message about imputation
  expect_message(
    result <- ensure_parent(test_nif),
    "PARENT field imputed"
  )

  # Check that PARENT was added
  expect_true("PARENT" %in% names(result))

  # For administrations, PARENT should equal ANALYTE
  admin_rows <- result[result$EVID == 1, ]
  expect_equal(admin_rows$PARENT, admin_rows$ANALYTE)
  expect_equal(unique(admin_rows$PARENT), "DRUG")

  # For observations, PARENT should be the analyte with lowest CMT (CMT 2 = "METABOLITE")
  obs_rows <- result[result$EVID == 0, ]
  expect_equal(unique(obs_rows$PARENT), "METABOLITE")

  expect_true(inherits(result, "nif"))
})


test_that("ensure_parent() Case 1b: Single treatment with multiple CMTs uses lowest", {
  # Test that lowest CMT is correctly identified when multiple analytes exist
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~DV,  ~AMT, ~ANALYTE,
    1,   0,     1,     1,    100,  100,  "DRUG",
    1,   1,     0,     5,    50,   0,    "CMT5",
    1,   2,     0,     3,    25,   0,    "CMT3",
    1,   3,     0,     2,    12,   0,    "CMT2"
  )

  test_nif <- nif(test_data)

  expect_message(
    result <- ensure_parent(test_nif),
    "PARENT field imputed"
  )

  # Lowest CMT is 2, so PARENT for observations should be "CMT2"
  obs_rows <- result[result$EVID == 0, ]
  expect_equal(unique(obs_rows$PARENT), "CMT2")
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
  # Multiple treatments and some subjects receive multiple treatments
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~DV,  ~AMT, ~ANALYTE,
    1,   0,     1,     1,    100,  100,  "DRUG_A",
    1,   24,    1,     1,    100,  100,  "DRUG_B",  # Same subject, different treatment
    1,   1,     0,     1,    50,   0,    "DRUG_A",
    1,   2,     0,     2,    25,   0,    "METABOLITE",
    2,   0,     1,     1,    100,  100,  "DRUG_A",
    2,   1,     0,     1,    50,   0,    "DRUG_A",
    2,   2,     0,     2,    25,   0,    "METABOLITE"
  )

  test_nif <- nif(test_data)

  # Should issue a warning about assumptions
  expect_warning(
    result <- ensure_parent(test_nif),
    "PARENT field imputed.*assumptions"
  )

  # Check that PARENT was added
  expect_true("PARENT" %in% names(result))

  # For administrations, PARENT should equal ANALYTE
  admin_rows <- result[result$EVID == 1, ]
  expect_equal(admin_rows$PARENT, admin_rows$ANALYTE)

  # For observations, PARENT should be the analyte with lowest CMT
  # Lowest CMT in observations is 1 (DRUG_A), so all observations should have PARENT = "DRUG_A"
  obs_rows <- result[result$EVID == 0, ]
  # Actually, we need to check what the lowest CMT analyte is
  # CMT 1 = "DRUG_A", CMT 2 = "METABOLITE", so lowest is CMT 1 = "DRUG_A"
  expect_equal(unique(obs_rows$PARENT), "DRUG_A")

  expect_true(inherits(result, "nif"))
})


test_that("ensure_parent() Case 2b: Multiple treatments with lowest CMT selection", {
  # Test that lowest CMT is correctly selected when subjects have multiple treatments
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~DV,  ~AMT, ~ANALYTE,
    1,   0,     1,     1,    100,  100,  "DRUG_A",
    1,   24,    1,     1,    100,  100,  "DRUG_B",
    1,   1,     0,     3,    50,   0,    "CMT3",
    1,   2,     0,     2,    25,   0,    "CMT2",  # Lowest CMT
    1,   3,     0,     4,    12,   0,    "CMT4"
  )

  test_nif <- nif(test_data)

  expect_warning(
    result <- ensure_parent(test_nif),
    "PARENT field imputed"
  )

  # Lowest CMT in observations is 2, so PARENT should be "CMT2"
  obs_rows <- result[result$EVID == 0, ]
  expect_equal(unique(obs_rows$PARENT), "CMT2")
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

  expect_error(
    ensure_parent(test_nif),
    "Cannot determine PARENT: no observation records"
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

  expect_error(
    ensure_parent(test_nif),
    "Cannot determine PARENT: no observation records"
  )
})


test_that("ensure_parent() errors with missing required columns", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~DV,  ~AMT,
    1,   0,     1,     1,    100,  100,
    1,   1,     0,     1,    50,   0
  ) |> nif()

  # Missing ID
  no_id <- test_data |>
    select(-"ID")

  expect_error(
    ensure_parent(no_id),
    "Missing required columns: ID"
  )

  # Missing EVID
  no_evid <- test_data |>
    select(-"EVID")

  expect_error(
    ensure_parent(no_evid),
    "Missing required columns: EVID"
  )

  # Missing CMT
  no_cmt <- test_data |>
    select(-"CMT")

  expect_error(
    ensure_parent(no_cmt),
    "Missing required columns: CMT"
  )

  # Missing multiple columns
  no_multiple <- test_data |>
    select(-c("ID", "CMT"))

  expect_error(
    ensure_parent(no_multiple),
    "Missing required columns"
  )
})


test_that("ensure_parent() errors with non-NIF object", {
  test_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~DV,  ~AMT,
    1,   0,     1,     1,    100,  100
  )

  # Not a NIF object
  expect_error(
    ensure_parent(test_data),
    "Input must be a nif object"
  )

  # Data frame converted from NIF
  test_nif <- nif(test_data)
  expect_error(
    ensure_parent(as.data.frame(test_nif)),
    "Input must be a nif object"
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
    # Subject 3: Multiple treatments (should trigger Case 2b)
    3,   0,     1,     1,    100,  100,  "DRUG_A",
    3,   24,    1,     1,    100,  100,  "DRUG_B",
    3,   1,     0,     2,    50,   0,    "METABOLITE",
    3,   2,     0,     3,    25,   0,    "METABOLITE2"
  )

  test_nif <- nif(test_data)

  expect_warning(
    result <- ensure_parent(test_nif),
    "PARENT field imputed"
  )

  # Subject 1: PARENT should be DRUG_A
  subj1_obs <- result[result$ID == 1 & result$EVID == 0, ]
  expect_equal(unique(subj1_obs$PARENT), "DRUG_A")

  # Subject 2: PARENT should be DRUG_B
  subj2_obs <- result[result$ID == 2 & result$EVID == 0, ]
  expect_equal(unique(subj2_obs$PARENT), "DRUG_B")

  # Subject 3: PARENT should be lowest CMT analyte (CMT 2 = "METABOLITE")
  subj3_obs <- result[result$ID == 3 & result$EVID == 0, ]
  expect_equal(unique(subj3_obs$PARENT), "METABOLITE")
})
