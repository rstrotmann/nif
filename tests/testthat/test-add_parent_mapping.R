# Test file for add_parent_mapping function

test_that("add_parent_mapping adds correct parent mapping", {
  # Create a basic SDTM object
  dm_data <- tibble::tribble(
    ~DOMAIN, ~STUDYID,   ~USUBJID,
       "DM", "STUDY1", "SUBJ-001"
  )

  # Create a new SDTM object
  test_sdtm <- new_sdtm(list(dm = dm_data))

  # Initial parent_mapping should be empty
  expect_equal(nrow(test_sdtm$parent_mapping), 0)

  # Add a parent mapping
  result <- add_parent_mapping(test_sdtm, "METABOLITE", "PARENT")

  # Check that the mapping was added
  expect_equal(nrow(result$parent_mapping), 1)
  expect_equal(result$parent_mapping$ANALYTE, "METABOLITE")
  expect_equal(result$parent_mapping$PARENT, "PARENT")
})


test_that("add_parent_mapping can add multiple mappings", {
  # Create a basic SDTM object
  dm_data <- tibble::tribble(
    ~DOMAIN, ~STUDYID,   ~USUBJID,
       "DM", "STUDY1", "SUBJ-001"
  )

  # Create a new SDTM object
  test_sdtm <- new_sdtm(list(dm = dm_data))

  # Add multiple parent mappings
  test_sdtm <- add_parent_mapping(test_sdtm, "METABOLITE1", "PARENT1")
  test_sdtm <- add_parent_mapping(test_sdtm, "METABOLITE2", "PARENT1")
  test_sdtm <- add_parent_mapping(test_sdtm, "METABOLITE3", "PARENT2")

  # Check that all mappings were added
  expect_equal(nrow(test_sdtm$parent_mapping), 3)

  # Check that the correct number of mappings exists for each parent
  parent1_mappings <- test_sdtm$parent_mapping[test_sdtm$parent_mapping$PARENT == "PARENT1", ]
  parent2_mappings <- test_sdtm$parent_mapping[test_sdtm$parent_mapping$PARENT == "PARENT2", ]

  expect_equal(nrow(parent1_mappings), 2)
  expect_equal(nrow(parent2_mappings), 1)
})


test_that("add_parent_mapping preserves existing mappings", {
  # Create a basic SDTM object with existing parent mapping
  dm_data <- tibble::tribble(
    ~DOMAIN, ~STUDYID,   ~USUBJID,
       "DM", "STUDY1", "SUBJ-001"
  )

  # Create a new SDTM object with existing parent mapping
  test_sdtm <- new_sdtm(list(dm = dm_data))
  test_sdtm$parent_mapping <- tibble::tribble(
    ~ANALYTE,  ~PARENT,
    "EXISTING", "EXISTING_PARENT"
  )

  # Add a new parent mapping
  result <- add_parent_mapping(test_sdtm, "NEW_METABOLITE", "NEW_PARENT")

  # Check that both the existing and new mappings are present
  expect_equal(nrow(result$parent_mapping), 2)
  expect_true("EXISTING" %in% result$parent_mapping$ANALYTE)
  expect_true("NEW_METABOLITE" %in% result$parent_mapping$ANALYTE)
})

test_that("add_parent_mapping validates input correctly", {
  # Test non-sdtm object
  expect_error(
    add_parent_mapping(data.frame(), "METABOLITE", "PARENT"),
    "'obj' must be an SDTM object"
  )

  # Create a valid SDTM object
  test_sdtm <- new_sdtm(list(dm = data.frame(DOMAIN = "DM", USUBJID = "SUBJ-001")))

  # Test empty analyte
  expect_error(
    add_parent_mapping(test_sdtm, "", "PARENT"),
    "'analyte' must be a non-empty character string"
  )

  # Test non-character analyte
  expect_error(
    add_parent_mapping(test_sdtm, 123, "PARENT"),
    "'analyte' must be a character string"
  )

  # Test empty parent
  expect_error(
    add_parent_mapping(test_sdtm, "METABOLITE", ""),
    "'parent' must be a non-empty character string"
  )

  # Test non-character parent
  expect_error(
    add_parent_mapping(test_sdtm, "METABOLITE", 123),
    "'parent' must be a character string"
  )
})

test_that("add_parent_mapping handles vector inputs with warnings", {
  # Create a valid SDTM object
  test_sdtm <- new_sdtm(list(dm = data.frame(DOMAIN = "DM", USUBJID = "SUBJ-001")))

  # Test vector for analyte
  expect_warning(
    result <- add_parent_mapping(test_sdtm, c("METABOLITE1", "METABOLITE2"), "PARENT"),
    "'analyte' has length > 1, using only the first element"
  )
  expect_equal(result$parent_mapping$ANALYTE, "METABOLITE1")

  # Test vector for parent
  expect_warning(
    result <- add_parent_mapping(test_sdtm, "METABOLITE3", c("PARENT1", "PARENT2")),
    "'parent' has length > 1, using only the first element"
  )
  expect_equal(result$parent_mapping$PARENT, "PARENT1")
})

test_that("add_parent_mapping rejects NA values", {
  # Create a valid SDTM object
  test_sdtm <- new_sdtm(list(dm = data.frame(DOMAIN = "DM", USUBJID = "SUBJ-001")))

  # Test NA for analyte
  expect_error(
    add_parent_mapping(test_sdtm, NA_character_, "PARENT"),
    "'analyte' cannot be NA"
  )

  # Test NA for parent
  expect_error(
    add_parent_mapping(test_sdtm, "METABOLITE", NA_character_),
    "'parent' cannot be NA"
  )
})

test_that("add_parent_mapping initializes parent_mapping when NULL", {
  # Create a basic SDTM object
  dm_data <- tibble::tribble(
    ~DOMAIN, ~STUDYID,   ~USUBJID,
       "DM", "STUDY1", "SUBJ-001"
  )

  # Create a new SDTM object
  test_sdtm <- new_sdtm(list(dm = dm_data))

  # Explicitly set parent_mapping to NULL
  test_sdtm$parent_mapping <- NULL

  # Add a parent mapping
  result <- add_parent_mapping(test_sdtm, "METABOLITE", "PARENT")

  # Check that the mapping was added
  expect_equal(nrow(result$parent_mapping), 1)
  expect_equal(result$parent_mapping$ANALYTE, "METABOLITE")
  expect_equal(result$parent_mapping$PARENT, "PARENT")
})

test_that("add_parent_mapping properly trims whitespace", {
  # Create a valid SDTM object
  test_sdtm <- new_sdtm(list(dm = data.frame(DOMAIN = "DM", USUBJID = "SUBJ-001")))

  # Add a parent mapping with spaces
  result <- add_parent_mapping(test_sdtm, "  METABOLITE  ", "  PARENT  ")

  # Check that whitespace was trimmed
  expect_equal(result$parent_mapping$ANALYTE, "METABOLITE")
  expect_equal(result$parent_mapping$PARENT, "PARENT")
})

