# Test file for add_analyte_mapping function

test_that("add_analyte_mapping adds correct mapping", {
  # Create a basic SDTM object
  dm_data <- tibble::tribble(
    ~DOMAIN, ~STUDYID, ~USUBJID,
    "DM", "STUDY1", "SUBJ-001"
  )

  # Create a new SDTM object
  test_sdtm <- new_sdtm(list(dm = dm_data))

  # Initial analyte_mapping should be empty
  expect_equal(nrow(test_sdtm$analyte_mapping), 0)

  # Add an analyte mapping
  result <- add_analyte_mapping(test_sdtm, "DRUG", "PK_MEASURE")

  # Check that the mapping was added
  expect_equal(nrow(result$analyte_mapping), 1)
  expect_equal(result$analyte_mapping$EXTRT, "DRUG")
  expect_equal(result$analyte_mapping$PCTESTCD, "PK_MEASURE")
  expect_equal(result$analyte_mapping$ANALYTE, "PK_MEASURE") # Default should use PCTESTCD as ANALYTE
})

test_that("add_analyte_mapping uses custom analyte name when provided", {
  # Create a basic SDTM object
  dm_data <- tibble::tribble(
    ~DOMAIN, ~STUDYID, ~USUBJID,
    "DM", "STUDY1", "SUBJ-001"
  )

  # Create a new SDTM object
  test_sdtm <- new_sdtm(list(dm = dm_data))

  # Add a mapping with custom analyte name
  result <- add_analyte_mapping(test_sdtm, "DRUG", "PK_MEASURE", "CUSTOM_NAME")

  # Check that the custom analyte name was used
  expect_equal(result$analyte_mapping$ANALYTE, "CUSTOM_NAME")
})

test_that("add_analyte_mapping enforces unique EXTRT", {
  # Create a basic SDTM object
  dm_data <- tibble::tribble(
    ~DOMAIN, ~STUDYID, ~USUBJID,
    "DM", "STUDY1", "SUBJ-001"
  )

  # Create a new SDTM object
  test_sdtm <- new_sdtm(list(dm = dm_data))

  # Add first mapping
  test_sdtm <- add_analyte_mapping(test_sdtm, "DRUG1", "PK_MEASURE1")

  # Adding mapping with different EXTRT should succeed
  expect_no_error(add_analyte_mapping(test_sdtm, "DRUG2", "PK_MEASURE2"))

  # Adding mapping with same EXTRT should fail
  expect_error(
    add_analyte_mapping(test_sdtm, "DRUG1", "PK_MEASURE3"),
    "EXTRT 'DRUG1' already exists in analyte mapping. EXTRT must be unique."
  )
})

test_that("add_analyte_mapping can add multiple mappings with different EXTRT", {
  # Create a basic SDTM object
  dm_data <- tibble::tribble(
    ~DOMAIN, ~STUDYID, ~USUBJID,
    "DM", "STUDY1", "SUBJ-001"
  )

  # Create a new SDTM object
  test_sdtm <- new_sdtm(list(dm = dm_data))

  # Add multiple analyte mappings with different EXTRT
  test_sdtm <- add_analyte_mapping(test_sdtm, "DRUG1", "PK_MEASURE1")
  test_sdtm <- add_analyte_mapping(test_sdtm, "DRUG2", "PK_MEASURE2")
  test_sdtm <- add_analyte_mapping(test_sdtm, "DRUG3", "PK_MEASURE3", "CUSTOM_NAME")

  # Check that all mappings were added
  expect_equal(nrow(test_sdtm$analyte_mapping), 3)

  # Check that mappings are correct
  expect_true("DRUG1" %in% test_sdtm$analyte_mapping$EXTRT)
  expect_true("DRUG2" %in% test_sdtm$analyte_mapping$EXTRT)
  expect_true("DRUG3" %in% test_sdtm$analyte_mapping$EXTRT)
  expect_true("PK_MEASURE1" %in% test_sdtm$analyte_mapping$PCTESTCD)
  expect_true("PK_MEASURE2" %in% test_sdtm$analyte_mapping$PCTESTCD)
  expect_true("PK_MEASURE3" %in% test_sdtm$analyte_mapping$PCTESTCD)
  expect_true("CUSTOM_NAME" %in% test_sdtm$analyte_mapping$ANALYTE)
})

test_that("add_analyte_mapping prevents updating existing EXTRT mappings", {
  # Create a basic SDTM object
  dm_data <- tibble::tribble(
    ~DOMAIN, ~STUDYID, ~USUBJID,
    "DM", "STUDY1", "SUBJ-001"
  )

  # Create a new SDTM object
  test_sdtm <- new_sdtm(list(dm = dm_data))

  # Add an initial mapping
  test_sdtm <- add_analyte_mapping(test_sdtm, "DRUG", "PK_MEASURE", "INITIAL_NAME")
  expect_equal(test_sdtm$analyte_mapping$ANALYTE, "INITIAL_NAME")

  # Attempt to update the mapping with same EXTRT should fail
  expect_error(
    add_analyte_mapping(test_sdtm, "DRUG", "PK_MEASURE", "UPDATED_NAME"),
    "EXTRT 'DRUG' already exists in analyte mapping. EXTRT must be unique."
  )
})

test_that("add_analyte_mapping validates input correctly", {
  # Test non-sdtm object
  expect_error(
    add_analyte_mapping(data.frame(), "DRUG", "PK_MEASURE"),
    "'obj' must be an SDTM object"
  )

  # Create a valid SDTM object
  test_sdtm <- new_sdtm(list(dm = data.frame(DOMAIN = "DM", USUBJID = "SUBJ-001")))

  # Test empty extrt
  expect_error(
    add_analyte_mapping(test_sdtm, "", "PK_MEASURE"),
    "'extrt' must be a non-empty character string"
  )

  # Test non-character extrt
  expect_error(
    add_analyte_mapping(test_sdtm, 123, "PK_MEASURE"),
    "'extrt' must be a character string"
  )

  # Test empty pctestcd
  expect_error(
    add_analyte_mapping(test_sdtm, "DRUG", ""),
    "'pctestcd' must be a non-empty character string"
  )

  # Test non-character pctestcd
  expect_error(
    add_analyte_mapping(test_sdtm, "DRUG", 123),
    "'pctestcd' must be a character string"
  )
})

test_that("add_analyte_mapping handles vector inputs with warnings", {
  # Create a valid SDTM object
  test_sdtm <- new_sdtm(list(dm = data.frame(DOMAIN = "DM", USUBJID = "SUBJ-001")))

  # Test vector for extrt
  expect_warning(
    result <- add_analyte_mapping(test_sdtm, c("DRUG1", "DRUG2"), "PK_MEASURE"),
    "'extrt' has length > 1, using only the first element"
  )
  expect_equal(result$analyte_mapping$EXTRT, "DRUG1")

  # Test vector for pctestcd
  expect_warning(
    result <- add_analyte_mapping(test_sdtm, "DRUG3", c("PK1", "PK2")),
    "'pctestcd' has length > 1, using only the first element"
  )
  expect_equal(result$analyte_mapping$PCTESTCD, "PK1")

  # Test vector for analyte
  expect_warning(
    result <- add_analyte_mapping(test_sdtm, "DRUG4", "PK3", c("A1", "A2")),
    "'analyte' has length > 1, using only the first element"
  )
  expect_equal(result$analyte_mapping$ANALYTE, "A1")
})

test_that("add_analyte_mapping rejects NA values", {
  # Create a valid SDTM object
  test_sdtm <- new_sdtm(list(dm = data.frame(DOMAIN = "DM", USUBJID = "SUBJ-001")))

  # Test NA for extrt
  expect_error(
    add_analyte_mapping(test_sdtm, NA_character_, "PK_MEASURE"),
    "'extrt' cannot be NA"
  )

  # Test NA for pctestcd
  expect_error(
    add_analyte_mapping(test_sdtm, "DRUG", NA_character_),
    "'pctestcd' cannot be NA"
  )

  # Test NA for analyte
  expect_error(
    add_analyte_mapping(test_sdtm, "DRUG", "PK_MEASURE", NA_character_),
    "'analyte' cannot be NA"
  )
})

test_that("add_analyte_mapping validates analyte parameter", {
  # Create a valid SDTM object
  test_sdtm <- new_sdtm(list(dm = data.frame(DOMAIN = "DM", USUBJID = "SUBJ-001")))

  # Test non-character analyte
  expect_error(
    add_analyte_mapping(test_sdtm, "DRUG", "PK_MEASURE", 123),
    "'analyte' must be a character string"
  )

  # Test empty analyte
  expect_error(
    add_analyte_mapping(test_sdtm, "DRUG", "PK_MEASURE", ""),
    "'analyte' must be a non-empty character string"
  )
})

test_that("add_analyte_mapping initializes analyte_mapping when NULL", {
  # Create a basic SDTM object
  dm_data <- tibble::tribble(
    ~DOMAIN, ~STUDYID, ~USUBJID,
    "DM", "STUDY1", "SUBJ-001"
  )

  # Create a new SDTM object
  test_sdtm <- new_sdtm(list(dm = dm_data))

  # Explicitly set analyte_mapping to NULL
  test_sdtm$analyte_mapping <- NULL

  # Add an analyte mapping
  result <- add_analyte_mapping(test_sdtm, "DRUG", "PK_MEASURE")

  # Check that the mapping was added
  expect_equal(nrow(result$analyte_mapping), 1)
  expect_equal(result$analyte_mapping$EXTRT, "DRUG")
  expect_equal(result$analyte_mapping$PCTESTCD, "PK_MEASURE")
  expect_equal(result$analyte_mapping$ANALYTE, "PK_MEASURE")
})

test_that("add_analyte_mapping properly trims whitespace", {
  # Create a valid SDTM object
  test_sdtm <- new_sdtm(list(dm = data.frame(DOMAIN = "DM", USUBJID = "SUBJ-001")))

  # Add an analyte mapping with spaces
  result <- add_analyte_mapping(test_sdtm, "  DRUG  ", "  PK_MEASURE  ", "  CUSTOM_NAME  ")

  # Check that whitespace was trimmed
  expect_equal(result$analyte_mapping$EXTRT, "DRUG")
  expect_equal(result$analyte_mapping$PCTESTCD, "PK_MEASURE")
  expect_equal(result$analyte_mapping$ANALYTE, "CUSTOM_NAME")
})
