# Tests for improved_guess_parent function

test_that("improved_guess_parent identifies most frequent parent correctly", {
  # Create a sample nif dataset with observations
  nif_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~ANALYTE, ~PARENT, ~METABOLITE, ~DV, ~CMT, ~MDV,
    1,   0,     1,     "DRUG1",  "DRUG1", FALSE,       NA,  1,    1,     # Administration
    1,   1,     0,     "DRUG1",  "DRUG1", FALSE,       10,  2,    0,     # Observation
    1,   2,     0,     "DRUG1",  "DRUG1", FALSE,       8,   2,    0,     # Observation
    1,   3,     0,     "DRUG1",  "DRUG1", FALSE,       6,   2,    0,     # Observation
    1,   1,     0,     "META1",  "DRUG1", TRUE,        2,   3,    0,     # Metabolite observation
    2,   0,     1,     "DRUG2",  "DRUG2", FALSE,       NA,  1,    1,     # Administration
    2,   1,     0,     "DRUG2",  "DRUG2", FALSE,       15,  2,    0,     # Observation
    2,   2,     0,     "DRUG2",  "DRUG2", FALSE,       12,  2,    0      # Observation
  )

  # Convert to nif object
  nif_obj <- nif_data %>%
    new_nif()

  # Test that improved_guess_parent correctly identifies DRUG1 (has 3 observations vs 2 for DRUG2)
  expect_equal(improved_guess_parent(nif_obj), "DRUG1")
})

test_that("improved_guess_parent returns NA for empty dataset", {
  # Create an empty nif object
  empty_nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~ANALYTE, ~PARENT, ~METABOLITE, ~DV, ~CMT, ~MDV
  ) %>%
    new_nif()

  # Test that improved_guess_parent returns NA for empty dataset
  # And issues a warning
  expect_warning(
    result <- improved_guess_parent(empty_nif),
    "Empty dataset"
  )
  expect_true(is.na(result))
})

test_that("improved_guess_parent can use custom default value", {
  # Create an empty nif object
  empty_nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~ANALYTE, ~PARENT, ~METABOLITE, ~DV, ~CMT, ~MDV
  ) %>%
    new_nif()

  # Test that improved_guess_parent returns the specified default value
  expect_warning(
    result <- improved_guess_parent(empty_nif, default_value = "DEFAULT_PARENT"),
    "Empty dataset"
  )
  expect_equal(result, "DEFAULT_PARENT")
})

test_that("improved_guess_parent can suppress warnings", {
  # Create an empty nif object
  empty_nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~ANALYTE, ~PARENT, ~METABOLITE, ~DV, ~CMT, ~MDV
  ) %>%
    new_nif()

  # Test that improved_guess_parent does not issue warnings when warn=FALSE
  expect_silent(
    result <- improved_guess_parent(empty_nif, warn = FALSE)
  )
  expect_true(is.na(result))
})

test_that("improved_guess_parent falls back to administrations when no observations exist", {
  # Create a sample nif dataset with administrations but no observations
  admin_only_nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~ANALYTE, ~PARENT, ~METABOLITE, ~DV, ~CMT, ~MDV,
    1,   0,     1,     "DRUG1",  "DRUG1", FALSE,       NA,  1,    1,     # Administration
    1,   24,    1,     "DRUG1",  "DRUG1", FALSE,       NA,  1,    1,     # Administration
    2,   0,     1,     "DRUG2",  "DRUG2", FALSE,       NA,  1,    1      # Administration
  ) %>%
    new_nif()

  # Test that improved_guess_parent falls back to administrations (DRUG1 has 2 vs DRUG2's 1)
  # And issues a warning about no observations and a message about using admin data
  expect_message(
    expect_warning(
      result <- improved_guess_parent(admin_only_nif),
      "No observations"
    ),
    "Parent determined from administration data"
  )
  expect_equal(result, "DRUG1")
})

test_that("improved_guess_parent can disable fallback to administrations", {
  # Create a sample nif dataset with administrations but no observations
  admin_only_nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~ANALYTE, ~PARENT, ~METABOLITE, ~DV, ~CMT, ~MDV,
    1,   0,     1,     "DRUG1",  "DRUG1", FALSE,       NA,  1,    1,     # Administration
    2,   0,     1,     "DRUG2",  "DRUG2", FALSE,       NA,  1,    1      # Administration
  ) %>%
    new_nif()

  # Test that improved_guess_parent returns NA when fallback is disabled
  expect_warning(
    result <- improved_guess_parent(admin_only_nif, fallback_to_admin = FALSE),
    "No observations"
  )
  expect_true(is.na(result))
})

test_that("improved_guess_parent returns NA when all observations are metabolites", {
  # Create a sample nif dataset with only metabolite observations
  nif_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~ANALYTE, ~PARENT, ~METABOLITE, ~DV, ~CMT, ~MDV,
    1,   0,     1,     "DRUG1",  "DRUG1", FALSE,       NA,  1,    1,     # Administration
    1,   1,     0,     "META1",  "DRUG1", TRUE,        2,   3,    0,     # Metabolite observation
    1,   2,     0,     "META1",  "DRUG1", TRUE,        3,   3,    0      # Metabolite observation
  ) %>%
    new_nif()

  # Test that improved_guess_parent returns NA when all observations are metabolites
  # And issues a warning
  expect_warning(
    result <- improved_guess_parent(nif_data),
    "Only metabolite observations found"
  )
  expect_true(is.na(result))
})

test_that("improved_guess_parent handles missing values correctly", {
  # Create a nif dataset with NA values
  nif_with_na <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~ANALYTE, ~PARENT, ~METABOLITE, ~DV, ~CMT, ~MDV,
    1,   0,     1,     "DRUG1",  "DRUG1", FALSE,       NA,  1,    1,     # Administration
    1,   1,     0,     "DRUG1",  "DRUG1", FALSE,       10,  2,    0,     # Observation
    1,   2,     0,     "DRUG1",  NA,      FALSE,       8,   2,    0,     # Observation with NA parent
    2,   0,     1,     "DRUG2",  "DRUG2", FALSE,       NA,  1,    1,     # Administration
    2,   1,     0,     "DRUG2",  "DRUG2", NA,          15,  2,    0      # Observation with NA metabolite
  ) %>%
    new_nif()

  # Test that improved_guess_parent can handle records with NA values
  expect_equal(improved_guess_parent(nif_with_na), "DRUG1")
})

test_that("improved_guess_parent integrates with ensure functions", {
  # Create a minimal dataset missing some fields
  minimal_nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~DV,
    1,   0,     1,     1,    NA,   # Administration
    1,   1,     0,     2,    10,   # Observation
    1,   2,     0,     2,    8     # Observation
  ) %>%
    new_nif()

  # The ensure_* functions should add missing fields
  # CMT 2 should become the default ANALYTE and PARENT
  result <- improved_guess_parent(minimal_nif)

  # Since CMT 2 is the observation compartment, we expect "2" as the guessed parent
  expect_equal(result, "2")
})
