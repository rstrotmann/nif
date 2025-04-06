# Test file for add_metabolite_mapping function

test_that("add_metabolite_mapping adds correct mapping", {
  # Create a basic SDTM object
  dm_data <- tibble::tribble(
    ~DOMAIN, ~STUDYID,   ~USUBJID,
       "DM", "STUDY1", "SUBJ-001"
  )

  # Create a new SDTM object
  test_sdtm <- new_sdtm(list(dm = dm_data))

  # Initial metabolite_mapping should be empty
  expect_equal(nrow(test_sdtm$metabolite_mapping), 0)

  # Add a metabolite mapping
  result <- add_metabolite_mapping(test_sdtm, "PARENT", "METABOLITE")

  # Check that the mapping was added
  expect_equal(nrow(result$metabolite_mapping), 1)
  expect_equal(result$metabolite_mapping$PCTESTCD_parent, "PARENT")
  expect_equal(result$metabolite_mapping$PCTESTCD_metab, "METABOLITE")
})


test_that("add_metabolite_mapping can add multiple mappings", {
  # Create a basic SDTM object
  dm_data <- tibble::tribble(
    ~DOMAIN, ~STUDYID,   ~USUBJID,
       "DM", "STUDY1", "SUBJ-001"
  )

  # Create a new SDTM object
  test_sdtm <- new_sdtm(list(dm = dm_data))

  # Add multiple metabolite mappings
  test_sdtm <- add_metabolite_mapping(test_sdtm, "PARENT1", "METABOLITE1")
  test_sdtm <- add_metabolite_mapping(test_sdtm, "PARENT2", "METABOLITE2")
  test_sdtm <- add_metabolite_mapping(test_sdtm, "PARENT1", "METABOLITE3")

  # Check that all mappings were added
  expect_equal(nrow(test_sdtm$metabolite_mapping), 3)

  # Check that mappings are correct
  expect_true("PARENT1" %in% test_sdtm$metabolite_mapping$PCTESTCD_parent)
  expect_true("PARENT2" %in% test_sdtm$metabolite_mapping$PCTESTCD_parent)
  expect_true("METABOLITE1" %in% test_sdtm$metabolite_mapping$PCTESTCD_metab)
  expect_true("METABOLITE2" %in% test_sdtm$metabolite_mapping$PCTESTCD_metab)
  expect_true("METABOLITE3" %in% test_sdtm$metabolite_mapping$PCTESTCD_metab)
})

test_that("add_metabolite_mapping prevents duplicate mappings", {
  # Create a basic SDTM object
  dm_data <- tibble::tribble(
    ~DOMAIN, ~STUDYID,   ~USUBJID,
       "DM", "STUDY1", "SUBJ-001"
  )

  # Create a new SDTM object
  test_sdtm <- new_sdtm(list(dm = dm_data))

  # Add an initial mapping
  test_sdtm <- add_metabolite_mapping(test_sdtm, "PARENT", "METABOLITE")

  # Attempt to add the same mapping again should fail
  expect_error(
    add_metabolite_mapping(test_sdtm, "PARENT", "METABOLITE"),
    "Metabolite mapping for parent 'PARENT' to metabolite 'METABOLITE' already exists."
  )
})

test_that("add_metabolite_mapping validates input correctly", {
  # Test non-sdtm object
  expect_error(
    add_metabolite_mapping(data.frame(), "PARENT", "METABOLITE"),
    "'obj' must be an SDTM object"
  )

  # Create a valid SDTM object
  test_sdtm <- new_sdtm(list(dm = data.frame(DOMAIN = "DM", USUBJID = "SUBJ-001")))

  # Test empty parent
  expect_error(
    add_metabolite_mapping(test_sdtm, "", "METABOLITE"),
    "'pctestcd_parent' must be a non-empty character string"
  )

  # Test non-character parent
  expect_error(
    add_metabolite_mapping(test_sdtm, 123, "METABOLITE"),
    "'pctestcd_parent' must be a character string"
  )

  # Test empty metabolite
  expect_error(
    add_metabolite_mapping(test_sdtm, "PARENT", ""),
    "'pctestcd_metabolite' must be a non-empty character string"
  )

  # Test non-character metabolite
  expect_error(
    add_metabolite_mapping(test_sdtm, "PARENT", 123),
    "'pctestcd_metabolite' must be a character string"
  )
})

test_that("add_metabolite_mapping handles vector inputs with warnings", {
  # Create a valid SDTM object
  test_sdtm <- new_sdtm(list(dm = data.frame(DOMAIN = "DM", USUBJID = "SUBJ-001")))

  # Test vector for parent
  expect_warning(
    result <- add_metabolite_mapping(test_sdtm, c("PARENT1", "PARENT2"), "METABOLITE"),
    "'pctestcd_parent' has length > 1, using only the first element"
  )
  expect_equal(result$metabolite_mapping$PCTESTCD_parent, "PARENT1")

  # Test vector for metabolite
  expect_warning(
    result <- add_metabolite_mapping(test_sdtm, "PARENT3", c("METABOLITE1", "METABOLITE2")),
    "'pctestcd_metabolite' has length > 1, using only the first element"
  )
  expect_equal(result$metabolite_mapping$PCTESTCD_metab, "METABOLITE1")
})

test_that("add_metabolite_mapping rejects NA values", {
  # Create a valid SDTM object
  test_sdtm <- new_sdtm(list(dm = data.frame(DOMAIN = "DM", USUBJID = "SUBJ-001")))

  # Test NA for parent
  expect_error(
    add_metabolite_mapping(test_sdtm, NA_character_, "METABOLITE"),
    "'pctestcd_parent' cannot be NA"
  )

  # Test NA for metabolite
  expect_error(
    add_metabolite_mapping(test_sdtm, "PARENT", NA_character_),
    "'pctestcd_metabolite' cannot be NA"
  )
})

test_that("add_metabolite_mapping initializes metabolite_mapping when NULL", {
  # Create a basic SDTM object
  dm_data <- tibble::tribble(
    ~DOMAIN, ~STUDYID,   ~USUBJID,
       "DM", "STUDY1", "SUBJ-001"
  )

  # Create a new SDTM object
  test_sdtm <- new_sdtm(list(dm = dm_data))

  # Explicitly set metabolite_mapping to NULL
  test_sdtm$metabolite_mapping <- NULL

  # Add a metabolite mapping
  result <- add_metabolite_mapping(test_sdtm, "PARENT", "METABOLITE")

  # Check that the mapping was added
  expect_equal(nrow(result$metabolite_mapping), 1)
  expect_equal(result$metabolite_mapping$PCTESTCD_parent, "PARENT")
  expect_equal(result$metabolite_mapping$PCTESTCD_metab, "METABOLITE")
})

test_that("add_metabolite_mapping properly trims whitespace", {
  # Create a valid SDTM object
  test_sdtm <- new_sdtm(list(dm = data.frame(DOMAIN = "DM", USUBJID = "SUBJ-001")))

  # Add a metabolite mapping with spaces
  result <- add_metabolite_mapping(test_sdtm, "  PARENT  ", "  METABOLITE  ")

  # Check that whitespace was trimmed
  expect_equal(result$metabolite_mapping$PCTESTCD_parent, "PARENT")
  expect_equal(result$metabolite_mapping$PCTESTCD_metab, "METABOLITE")
})

