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
