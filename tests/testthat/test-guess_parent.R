# Tests for guess_parent function

test_that("guess_parent identifies analyte with most administrations", {
  # Create a sample nif dataset with administrations
  nif_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~ANALYTE, ~PARENT, ~METABOLITE, ~DV, ~CMT, ~MDV,
    1,   0,     1,     "DRUG1",  "DRUG1", FALSE,       NA,  1,    1,     # Administration
    1,   24,    1,     "DRUG1",  "DRUG1", FALSE,       NA,  1,    1,     # Another administration
    2,   0,     1,     "DRUG2",  "DRUG2", FALSE,       NA,  1,    1      # Administration of different drug
  )

  # Convert to nif object
  obj <- nif_data %>%
    new_nif()

  # Test that guess_parent correctly identifies DRUG1 (has 2 administrations vs 1 for DRUG2)
  expect_equal(guess_parent(obj), "DRUG1")
})


test_that("guess_parent falls back to observations when no administrations exist", {
  # Create a sample nif dataset with only observations (no administrations)
  nif_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~ANALYTE, ~PARENT, ~METABOLITE, ~DV, ~CMT, ~MDV,
    1,   1,     0,     "DRUG1",  "DRUG1", FALSE,       10,  2,    0,     # Observation
    1,   2,     0,     "DRUG1",  "DRUG1", FALSE,       8,   2,    0,     # Observation
    1,   3,     0,     "DRUG1",  "DRUG1", FALSE,       6,   2,    0,     # Observation
    2,   1,     0,     "DRUG2",  "DRUG2", FALSE,       15,  2,    0,     # Observation
    2,   2,     0,     "DRUG2",  "DRUG2", FALSE,       12,  2,    0      # Observation
  )

  # Convert to nif object
  obj <- nif_data %>%
    new_nif()

  # Test that guess_parent correctly identifies DRUG1 (has 3 observations vs 2 for DRUG2)
  expect_equal(guess_parent(obj), "DRUG1")
})


test_that("guess_parent ignores metabolite observations", {
  # Create a sample nif dataset with mixed metabolite and parent observations
  nif_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~ANALYTE, ~PARENT, ~METABOLITE, ~DV, ~CMT, ~MDV,
    1,   1,     0,     "DRUG1",  "DRUG1", FALSE,       10,  2,    0,     # Parent observation
    1,   2,     0,     "META1",  "DRUG1", TRUE,        8,   3,    0,     # Metabolite observation
    1,   3,     0,     "META1",  "DRUG1", TRUE,        6,   3,    0,     # Metabolite observation
    2,   1,     0,     "DRUG2",  "DRUG2", FALSE,       15,  2,    0,     # Parent observation
    2,   2,     0,     "DRUG2",  "DRUG2", FALSE,       12,  2,    0      # Parent observation
  )

  # Convert to nif object
  obj <- nif_data %>%
    new_nif()

  # Test that guess_parent correctly identifies DRUG2 (has 2 parent observations vs 1 for DRUG1)
  # It should ignore the metabolite observations for META1
  expect_equal(guess_parent(obj), "DRUG2")
})


test_that("guess_parent prioritizes administrations over observations", {
  # Create a sample nif dataset with both administrations and observations
  nif_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~ANALYTE, ~PARENT, ~METABOLITE, ~DV, ~CMT, ~MDV,
    1,   0,     1,     "DRUG1",  "DRUG1", FALSE,       NA,  1,    1,     # Administration
    2,   0,     1,     "DRUG2",  "DRUG2", FALSE,       NA,  1,    1,     # Administration
    2,   24,    1,     "DRUG2",  "DRUG2", FALSE,       NA,  1,    1,     # Administration
    1,   1,     0,     "DRUG1",  "DRUG1", FALSE,       10,  2,    0,     # Observation
    1,   2,     0,     "DRUG1",  "DRUG1", FALSE,       8,   2,    0,     # Observation
    1,   3,     0,     "DRUG1",  "DRUG1", FALSE,       6,   2,    0      # Observation
  )

  # Convert to nif object
  obj <- nif_data %>%
    new_nif()

  # Test that guess_parent prioritizes administrations over observations
  # Even though DRUG1 has 3 observations and DRUG2 has 0 observations,
  # DRUG2 has 2 administrations vs DRUG1's 1, so DRUG2 should be returned
  expect_equal(guess_parent(obj), "DRUG2")
})


test_that("guess_parent returns NULL for empty dataset", {
  # Create an empty nif object
  empty_nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~ANALYTE, ~PARENT, ~METABOLITE, ~DV, ~CMT, ~MDV
  ) %>%
    new_nif()

  # Test that guess_parent returns NULL for empty dataset
  expect_null(guess_parent(empty_nif))
})


test_that("guess_parent works with minimal dataset", {
  # Create a minimal dataset with only required fields
  minimal_nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~ANALYTE,
    1,   0,     1,     "DRUG1",
    1,   24,    1,     "DRUG1",
    2,   0,     1,     "DRUG2"
  ) %>%
    new_nif()

  # Test that guess_parent works with minimal data
  expect_equal(guess_parent(minimal_nif), "DRUG1")
})


test_that("guess_parent handles tied administration counts", {
  # Create a dataset with tied administration counts
  tied_nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~ANALYTE, ~PARENT, ~METABOLITE, ~DV, ~CMT, ~MDV,
    1,   0,     1,     "DRUG1",  "DRUG1", FALSE,       NA,  1,    1,     # Administration
    2,   0,     1,     "DRUG2",  "DRUG2", FALSE,       NA,  1,    1      # Administration
  ) %>%
    new_nif()

  # The function should return DRUG1 for ties (alphabetical sorting)
  expect_equal(guess_parent(tied_nif), "DRUG1")
})


test_that("guess_parent works with ensure_analyte", {
  # Create a dataset missing ANALYTE field (should be added by ensure_analyte)
  missing_analyte_nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~DV, ~MDV,
    1,   0,     1,     1,    NA,  1,     # Administration
    2,   0,     1,     1,    NA,  1      # Administration
  ) %>%
    new_nif()

  # The function should add ANALYTE based on CMT
  result <- guess_parent(missing_analyte_nif)

  # Since ANALYTE should be set to "1" for both records, should return "1"
  expect_equal(result, "CMT1")
})


test_that("guess_parent returns NULL for dataset with only metabolite observations", {
  # Create a dataset with only metabolite observations
  metabolite_only_nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~ANALYTE, ~PARENT, ~METABOLITE, ~DV, ~CMT, ~MDV,
    1,   1,     0,     "META1",  "DRUG1", TRUE,        2,   3,    0,     # Metabolite observation
    1,   2,     0,     "META1",  "DRUG1", TRUE,        3,   3,    0      # Metabolite observation
  ) %>%
    new_nif()

  # Test that guess_parent returns NULL when it can't determine parent
  expect_null(guess_parent(metabolite_only_nif))
})


test_that("guess_parent handles NA values in key columns", {
  # Create a dataset with NA values in key columns
  na_values_nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~ANALYTE, ~PARENT, ~METABOLITE, ~DV, ~CMT, ~MDV,
    1,   0,     1,     NA,       "DRUG1", FALSE,       NA,  1,    1,     # Administration with NA analyte
    1,   24,    1,     "DRUG1",  "DRUG1", FALSE,       NA,  1,    1,     # Administration
    2,   0,     1,     "DRUG2",  "DRUG2", FALSE,       NA,  1,    1      # Administration
  ) %>%
    new_nif()

  # Test how guess_parent handles NA values in ANALYTE
  # Should ignore records with NA ANALYTE
  expect_equal(guess_parent(na_values_nif), "DRUG1")
})


test_that("guess_parent correctly counts tied observations when no administrations exist", {
  # Create a dataset with no administrations and tied observation counts
  tied_obs_nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~ANALYTE, ~PARENT, ~METABOLITE, ~DV, ~CMT, ~MDV,
    1,   1,     0,     "DRUG1",  "DRUG1", FALSE,       10,  2,    0,     # Observation
    1,   2,     0,     "DRUG1",  "DRUG1", FALSE,       8,   2,    0,     # Observation
    2,   1,     0,     "DRUG2",  "DRUG2", FALSE,       15,  2,    0,     # Observation
    2,   2,     0,     "DRUG2",  "DRUG2", FALSE,       12,  2,    0      # Observation
  ) %>%
    new_nif()

  # Test that guess_parent handles tied observation counts correctly
  # Should alphabetically sort when tied, returning DRUG1
  expect_equal(guess_parent(tied_obs_nif), "DRUG1")
})
