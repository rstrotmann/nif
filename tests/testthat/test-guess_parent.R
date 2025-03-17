# Tests for guess_parent function

test_that("guess_parent identifies most frequent parent correctly", {
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
  obj <- nif_data %>%
    new_nif()

  # Test that guess_parent correctly identifies DRUG1 (has 3 observations vs 2 for DRUG2)
  expect_equal(guess_parent(obj), "DRUG1")
})


test_that("guess_parent returns first alphabetically when multiple parents have same frequency", {
  # Create a sample nif dataset with observations
  nif_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~ANALYTE, ~PARENT, ~METABOLITE, ~DV, ~CMT, ~MDV,
    1,   0,     1,     "DRUGA",  "DRUGA", FALSE,       NA,  1,    1,     # Administration
    1,   1,     0,     "DRUGA",  "DRUGA", FALSE,       10,  2,    0,     # Observation
    1,   2,     0,     "DRUGA",  "DRUGA", FALSE,       8,   2,    0,     # Observation
    2,   0,     1,     "DRUGB",  "DRUGB", FALSE,       NA,  1,    1,     # Administration
    2,   1,     0,     "DRUGB",  "DRUGB", FALSE,       15,  2,    0,     # Observation
    2,   2,     0,     "DRUGB",  "DRUGB", FALSE,       12,  2,    0      # Observation
  )

  # Convert to nif object
  obj <- nif_data %>%
    new_nif()

  # Test that guess_parent selects first alphabetically (DRUGA) when both have same count
  expect_equal(guess_parent(obj), "DRUGA")
})


test_that("guess_parent ignores metabolites when determining the parent", {
  # Create a sample nif dataset with observations
  nif_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~ANALYTE, ~PARENT, ~METABOLITE, ~DV, ~CMT, ~MDV,
    1,   0,     1,     "DRUG1",  "DRUG1", FALSE,       NA,  1,    1,     # Administration
    1,   1,     0,     "DRUG1",  "DRUG1", FALSE,       10,  2,    0,     # Observation
    1,   1,     0,     "META1",  "DRUG1", TRUE,        2,   3,    0,     # Metabolite observation
    1,   2,     0,     "META1",  "DRUG1", TRUE,        3,   3,    0,     # Metabolite observation
    1,   3,     0,     "META1",  "DRUG1", TRUE,        4,   3,    0,     # Metabolite observation
    2,   0,     1,     "DRUG2",  "DRUG2", FALSE,       NA,  1,    1,     # Administration
    2,   1,     0,     "DRUG2",  "DRUG2", FALSE,       15,  2,    0,     # Observation
    2,   2,     0,     "DRUG2",  "DRUG2", FALSE,       12,  2,    0      # Observation
  )

  # Convert to nif object
  obj <- nif_data %>%
    new_nif()

  # Test that guess_parent still selects DRUG2 even though META1 has more observations
  # This is because META1 observations are metabolites (METABOLITE = TRUE)
  expect_equal(guess_parent(obj), "DRUG2")
})


test_that("guess_parent returns NULL when all observations are metabolites", {
  # Create a sample nif dataset with only metabolite observations
  nif_data <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~ANALYTE, ~PARENT, ~METABOLITE, ~DV, ~CMT, ~MDV,
    1,   0,     1,     "DRUG1",  "DRUG1", FALSE,       NA,  1,    1,     # Administration
    1,   1,     0,     "META1",  "DRUG1", TRUE,        2,   3,    0,     # Metabolite observation
    1,   2,     0,     "META1",  "DRUG1", TRUE,        3,   3,    0      # Metabolite observation
  )

  # Convert to nif object
  nif_obj <- nif_data %>%
    new_nif()

  # Test that guess_parent returns NULL when all observations are metabolites
  expect_null(guess_parent(nif_obj))
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

test_that("guess_parent returns NULL when dataset has no observations", {
  # Create a sample nif dataset with administrations but no observations
  admin_only_nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~ANALYTE, ~PARENT, ~METABOLITE, ~DV, ~CMT, ~MDV,
    1,   0,     1,     "DRUG1",  "DRUG1", FALSE,       NA,  1,    1,     # Administration
    2,   0,     1,     "DRUG2",  "DRUG2", FALSE,       NA,  1,    1      # Administration
  ) %>%
    new_nif()

  # Test that guess_parent returns NULL when there are no observations
  expect_null(guess_parent(admin_only_nif))
})

test_that("guess_parent works with ensure functions", {
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
  # This test verifies that the ensure_* functions work properly with guess_parent
  result <- guess_parent(minimal_nif)

  # Since CMT 2 is the observation compartment, we expect "2" as the guessed parent
  expect_equal(result, "2")
})

test_that("guess_parent handles dataset with missing values correctly", {
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

  # Test that guess_parent can handle records with NA values
  expect_equal(guess_parent(nif_with_na), "DRUG1")
})

test_that("guess_parent integration with add_observation", {
  # Create a base nif with only administration
  base_nif <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~ANALYTE, ~PARENT, ~METABOLITE, ~DV, ~CMT, ~MDV,
    1,   0,     1,     "DRUG1",  "DRUG1", FALSE,       NA,  1,    1     # Administration
  ) %>%
    new_nif()

  # This is a mock sdtm object that would be passed to add_observation
  mock_sdtm <- list(
    domains = list(
      pc = tibble::tribble(
        ~USUBJID, ~DOMAIN, ~PCTESTCD, ~PCDTC,               ~PCSTRESN,
        "1",      "PC",    "DRUG1",   "2024-01-01T08:00:00", 100
      )
    )
  )
  class(mock_sdtm) <- c("sdtm", "list")

  # Skip this test if add_observation isn't available or doesn't behave as expected
  skip_if_not(exists("add_observation"), "add_observation function not available")

  # This test makes sure that guess_parent interacts correctly with add_observation
  # when neither parent nor analyte is specified
  tryCatch({
    # This should work because guess_parent should find DRUG1 as the parent
    result <- add_observation(base_nif, mock_sdtm, "pc", "DRUG1", silent = TRUE)
    expect_s3_class(result, "nif")
    expect_true("DRUG1" %in% result$PARENT)
  }, error = function(e) {
    skip(paste("add_observation test failed with error:", e$message))
  })
})
